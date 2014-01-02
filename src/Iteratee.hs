--
--
module Main where

{-# LANGUAGE Rank2Types  #-}

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

type Enumerator a = Iter a -> IO (Result a)

type Inum a = Iter a -> Iter (Result a)

(.|) :: Inum a -> Iter a -> Iter a
(.|) inum iter = inum iter >>= getResult
infixr 4 .|

run :: (MonadIO m) => Iter a -> m a
run = getResult . NeedInput

xargsCat :: Inum a
xargsCat iter = do
  mpath <- readLine
  case mpath of
    Nothing   -> return (NeedInput iter)
    Just path -> inumFile0 (L8.unpack path) `cat` xargsCat $ iter

cat :: Inum a -> Inum a -> Inum a
cat a b iter = a iter >>= check
    where check (NeedInput iter') = b iter'
          check (NeedIO io)       = liftIO io >>= check
          check r                 = return r

inumFile0 :: FilePath -> Inum a
inumFile0 path iter = liftIO $ enumerateFile path iter

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

countLines0 :: FilePath -> IO Int
countLines0 dir = do
    stat <- getSymbolicLinkStatus dir

    let enumerator files = foldr cat0 enumerateNull $
                           map enumerateFile files
        result e = e nlines1 >>= getResult

    if isRegularFile stat
       then result $ enumerator [dir]
       else recDir dir >>= result . enumerator

enumerateNull :: Enumerator a
enumerateNull = return . NeedInput

cat0 :: Enumerator a -> Enumerator a -> Enumerator a
cat0 a b iter = a iter >>= check
    where check (NeedInput iter') = b iter'
          check (NeedIO io)       = io >>= check
          check r                 = return r

nlines1 :: Iter Int
nlines1 = go 0
  where
      go n = let check (Just _) = go $ n + 1
                 check Nothing  = return n
             in readLine >>= check

nlines0 :: Iter Int
nlines0 = Iter (go 0)
    where go n c0 = check (runIter readLine c0)
              where
                check (NeedInput (Iter f)) =
                    NeedInput (Iter (check . f))
                check (Done (Just _) c) = go (n + 1) c
                check (Done Nothing c)  = Done n c
                check (NeedIO r)        = NeedIO (liftM check r)
                check (Failed e)        = Failed e

enumerateFile :: FilePath -> Enumerator a
enumerateFile path iter0 =
    bracket (openFile path ReadMode) hClose $ \h ->
    let go iter = do
          input <- S.hGetSome h 32752
          if S.null input
            then return (NeedInput iter)
            else check $ runIter iter $
                 Chunk (L.fromChunks [input]) False
        check (NeedInput iter) = go iter
        check (NeedIO iter)    = iter >>= check
        check result           = return result
    in go iter0

readLine :: Iter (Maybe L.ByteString)
readLine = Iter (go L.empty)
    where go acc (Chunk input eof)
              | not (L.null b) = Done (Just acca) (Chunk btail eof)
              | not eof        = NeedInput (Iter (go acca))
              | otherwise      = Done Nothing (Chunk acca eof)
              where (a, b) = L8.break (== '\n') input
                    acca   = L.append acc a
                    btail  = L.tail b

chunkEOF :: Chunk
chunkEOF = Chunk L.empty True

getResult :: (MonadIO m) => Result a -> m a
getResult (Done a _)           = return a
getResult (NeedInput (Iter f)) = getResult (f chunkEOF)
getResult (NeedIO io)          = liftIO io >>= getResult
getResult (Failed e)           = liftIO $ throwIO e
