--
--
{-# LANGUAGE RankNTypes #-}

module Inum where

import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as L8

import Control.Monad
import Control.Monad.Trans
import Control.Exception
import Data.Monoid
import System.FilePath
import System.Posix
import System.IO

data Chunk = Chunk { chunkData :: !L.ByteString
                   , chunkAtEOF :: !Bool } deriving (Show)

newtype Iter a = Iter { runIter :: Chunk -> Result a }

data Result a = Done { rResult :: a, rResidual :: Chunk }
              | NeedInput !(Iter a)
              | NeedIO !(IO (Result a))
              | Failed !SomeException

instance MonadIO Iter where
    liftIO io = Iter $ \c -> NeedIO $ try io >>= mkResult c
        where mkResult _ (Left e)  = return (Failed e)
              mkResult c (Right a) = return (Done a c)

instance Monoid Chunk where
    mempty = chunkEOF
    (Chunk xs e1) `mappend` (Chunk ys e2) =
        Chunk (L.append xs ys) (e1 || e2)

instance MonadPlus Iter where
    mzero = fail "mzero"
    mplus itera0 iterb = go mempty itera0
        where go acc itera = Iter $ \c ->
                  let acc' = mappend acc c
                      check (NeedInput i) = NeedInput (go acc' i)
                      check (NeedIO io) = NeedIO (liftM check io)
                      check (Failed _) = runIter iterb acc'
                      check r = r
                  in check $ runIter itera c

instance Monad Iter where
    return a = Iter $ Done a
    m >>= k = Iter $ \c -> check (runIter m c)
        where check (Done a c')     = runIter (k a) c'
              check (NeedInput m') = NeedInput (m' >>= k)
              check (NeedIO io)    = NeedIO (liftM check io)
              check (Failed e)     = Failed e
    fail msg = iterThrow (ErrorCall msg)

iterThrow :: (Exception e) => e -> Iter a
iterThrow e = Iter $ \_ -> Failed (toException e)

type Inum a = Iter a -> Iter (Result a)

type Codec a = Iter (L.ByteString, Maybe (Inum a))

enumDir :: FilePath -> Inum a
enumDir dir = inumBracket (liftIO $ openDirStream dir)
              (liftIO . closeDirStream) $ \ds ->
  let inum = runCodec nextName
      nextName = liftIO (readDirStream ds) >>= checkName

      checkName "" = return (L.empty, Nothing)
      checkName "." = nextName
      checkName ".." = nextName
      checkName name = liftIO (getSymbolicLinkStatus path)
                       >>= checkStat path
          where path = dir </> name

      checkStat path stat
          | isRegularFile stat =
              return (L8.pack $ path ++ "\n", Just inum)
          | isDirectory stat =
              return (L.empty, Just $ enumDir path `cat` inum)
          | otherwise = nextName
  in inum

inumFile  :: FilePath -> Inum a
inumFile path = inumBracket (liftIO $ openFile path ReadMode)
                (liftIO . hClose) $ \h ->
    let inum = runCodec $ do
          input <- liftIO $ S.hGetSome h 32752
          let next = if S.null input then Nothing else Just inum
          return (L.fromChunks [input], next)
    in inum

inumPure :: L.ByteString -> Inum a
inumPure buf (Iter f) = return (f (Chunk buf False))

runCodec :: Codec a -> Inum a
runCodec codec iter = do
  (input, mNext) <- codec
  maybe (inumPure input) (inumPure input `cat`) mNext $ iter

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
    Just path -> enumerateFile (L8.unpack path) `cat` xargsCat $ iter

cat :: Inum a -> Inum a -> Inum a
cat a b iter = a iter >>= check
    where check (NeedInput iter') = b iter'
          check (NeedIO io)       = liftIO io >>= check
          check r                 = return r

enumerateFile :: FilePath -> Inum a
enumerateFile path iter0 = liftIO $
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

countLines :: Iter Int
countLines = go 0
  where go n = readLine >>= maybe (return n)
                            (\_ -> go $! n + 1)

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
