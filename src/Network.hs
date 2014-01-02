--

import Control.Applicative
import Control.Monad.State
import qualified Data.Map as Map

type Address = String

data Number = N !(Map.Map Address Int) !Int
              deriving (Show)

renumber :: [(Address,Address)] -> [(Int,Int)]
renumber xs = evalState (mapM pair xs) (N Map.empty 0)
  where pair (x,y) = (,) <$> number x <*> number y

number :: Address -> State Number Int
number addr = do
  N numMap highest <- get
  case Map.lookup addr numMap of
    Just j  -> return j
    Nothing -> do let highest' = highest + 1
                      newMap = Map.insert addr highest numMap
                  put $! N newMap highest'
                  return highest
