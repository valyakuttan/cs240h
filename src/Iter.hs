--
--
{-# LANGUAGE RankNTypes #-}

module Iter where

import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as L8

import Control.Monad
import Control.Monad.Trans
import Control.Exception
import System.FilePath
import System.Posix
import System.IO

import CountLines

data Chunk = Chunk { chunkData :: !L.ByteString
                   , chunkAtEOF :: !Bool } deriving (Show)

newtype Iter a = Iter { runIter :: Chunk -> Result a }

data Result a = Done { rResult :: a, rResidual :: Chunk }
              | NeedInput !(Iter a)
              | NeedIO !(IO (Result a))
              | Failed !SomeException

type Enumerator a = Iter a -> IO (Result a)

instance Monad Iter where
    return a = Iter $ Done a
    m >>= k = Iter $ \c -> check (runIter m c)
        where check (Done a c')     = runIter (k a) c'
              check (NeedInput m') = NeedInput (m' >>= k)
              check (NeedIO io)    = NeedIO (liftM check io)
              check (Failed e)     = Failed e
    fail msg = iterThrow (ErrorCall msg)

instance MonadIO Iter where
    liftIO io = Iter $ \c -> NeedIO $ try io >>= mkResult c
        where mkResult _ (Left e)  = return (Failed e)
              mkResult c (Right a) = return (Done a c)

iterThrow :: (Exception e) => e -> Iter a
iterThrow e = Iter $ \_ -> Failed (toException e)

instance Show a => Show (Result a) where
    show (Done a c) = show a ++ " " ++ show c
    show (NeedInput _) = "NeedInput"
    show (NeedIO _) = "NeedIO"
    show _          = "Failed"

type Inum a = Iter a -> Iter (Result a)

iterCatch :: Iter a -> (SomeException -> Iter a) -> Iter a
iterCatch (Iter f0) handler = Iter (check . f0)
    where check (NeedInput (Iter f)) = NeedInput (Iter (check . f))
          check (NeedIO io)          = NeedIO (liftM check io)
          check (Failed e)           = NeedInput (handler e)
          check done                 = done

onFailed :: Iter a -> Iter b -> Iter a
onFailed iter cleanup = iter `iterCatch` \e -> cleanup >> iterThrow e

iterBracket :: Iter a -> (a -> Iter b) -> (a -> Iter c) -> Iter c
iterBracket before after action = do
  a <- before
  b <- action a `onFailed` after a
  after a
  return b

inumBracket :: Iter a -> (a -> Iter b) -> (a -> Inum c) -> Inum c
inumBracket before after inum iter =
    iterBracket before after (flip inum iter)

xargsCat :: Inum a
xargsCat iter = do
  mpath <- readLine
  case mpath of
    Nothing   -> return (NeedInput iter)
    Just path -> inumFile (L8.unpack path) `cat` xargsCat $ iter

cat :: Inum a -> Inum a -> Inum a
cat a b iter = a iter >>= check
    where check (NeedInput iter') = b iter'
          check (NeedIO io)       = liftIO io >>= check
          check r                 = return r

inumFile :: FilePath -> Inum a
inumFile path iter = liftIO $ enumerateFile path iter

enumerateNull :: Enumerator a
enumerateNull = return . NeedInput

cat0 :: Enumerator a -> Enumerator a -> Enumerator a
cat0 a b iter = a iter >>= check
    where check (NeedInput iter') = b iter'
          check (NeedIO io)       = io >>= check
          check r                 = return r

countLines0 :: FilePath -> IO Int
countLines0 dir = do
    stat <- getSymbolicLinkStatus dir

    let enumerator files = foldr cat0 enumerateNull $
                           map enumerateFile files
        result e = e nLines >>= getResult

    if isRegularFile stat
       then result $ enumerator [dir]
       else recDir dir >>= result . enumerator

-- Return chunk that is non-empty of has EOF set
iterChunk :: Iter Chunk
iterChunk = Iter $ \c@(Chunk buf eof) ->
            if L.null buf && not eof
            then NeedInput iterChunk
            else Done c (Chunk L.empty eof)

-- Dump input to standard output
iterStdout :: Iter ()
iterStdout = do
  (Chunk buf eof) <- iterChunk
  liftIO $ L.putStr buf
  unless eof iterStdout

chunkEOF :: Chunk
chunkEOF = Chunk L.empty True

getResult :: (MonadIO m) => Result a -> m a
getResult (Done a _)           = return a
getResult (NeedInput (Iter f)) = getResult (f chunkEOF)
getResult (NeedIO io)          = liftIO io >>= getResult
getResult (Failed e)           = liftIO $ throwIO e

nLines :: Iter Int
nLines = go 0
  where go n = readLine >>= maybe (return n)
                            (\_ -> go $ n + 1)

readLine :: Iter (Maybe L.ByteString)
readLine = go L.empty
  where go acc = let check Nothing  =
                         if L.null acc
                             then return Nothing
                             else return $ Just acc
                     check (Just c) =
                         if c == '\n'
                             then return $ Just acc
                             else go $ L8.snoc acc c
                 in readChar >>= check

nChars :: Iter (Int)
nChars = go 0
  where go n = readChar >>= maybe (return n)
                            (\_ -> go $ n + 1)

readChar ::Iter (Maybe Char)
readChar = Iter (go Nothing)
  where go mc (Chunk input eof)
           | not (L.null input) = Done (Just c) (Chunk rest eof)
           | not eof            = NeedInput (Iter (go mc))
           | otherwise          = Done Nothing (Chunk xs eof)
           where rest = L.tail input
                 c    = L8.head input
                 xs   =L8.pack $ maybe [] (:[]) $ mc

enumerateFile :: FilePath -> Enumerator a
enumerateFile path iter0 =
    bracket (openFile path ReadMode) hClose $ \h -> do
    let go iter = do
          input <- S.hGetSome h 32752
          if S.null input
            then return (NeedInput iter)
            else check $ runIter iter $
                 Chunk (L.fromChunks [input]) False
        check (NeedInput iter) = go iter
        check (NeedIO iter)    = iter >>= check
        check result           = return result
    go iter0
