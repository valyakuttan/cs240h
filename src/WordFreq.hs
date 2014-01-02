--
--

import Control.Monad (forM_)
import Data.List (sortBy, foldl')
import Data.Ord (comparing)
import System.Environment (getArgs)
import qualified Data.Map as M

main = do
  args <- getArgs
  forM_ args $ \f -> do
    ws <- words `fmap` readFile f
    return ()
    forM_ 
    (sortBy (comparing snd) . M.toList .
     foldl' (\m w -> M.insertWith (+) w 1 m) M.empty $ ws) $
          \(w,c) -> putStrLn $ show c ++ "\t" ++ w

