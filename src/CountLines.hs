-- CountLines.hs

module CountLines (recDir) where

import Control.Monad
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as L8
import System.FilePath
import System.Posix
import System.IO.Unsafe    -- for understanding, not recommended

countLines :: FilePath -> IO ()
countLines dir =
    recDir dir >>= readFiles >>= print . L8.count '\n'

readFiles :: [FilePath] -> IO L.ByteString
readFiles [] = return L.empty
readFiles (f:fs) = liftM2 L.append (L.readFile f)
                   (unsafeInterleaveIO $ readFiles fs)

recDir :: FilePath -> IO [FilePath]
recDir dir = do
  ds <- openDirStream dir

  let nextName = unsafeInterleaveIO $ readDirStream ds >>= checkName

      checkName "" = closeDirStream ds >> return []
      checkName "." = nextName
      checkName ".." = nextName
      checkName name = getSymbolicLinkStatus path >>= checkStat path
          where path = dir </> name

      checkStat path stat
          | isRegularFile stat = liftM (path :) nextName
          | isDirectory stat   = liftM2 (++) (recDir path) nextName
          | otherwise          = nextName

  nextName

