--

import qualified Data.Map as Map

sieve :: [Integer] -> [Integer]
sieve xs = sieve' xs Map.empty
  where
      sieve' [] table = []
      sieve' (x:xs) table = case Map.lookup x table of
          Nothing    -> x : sieve' xs (Map.insert (x*x) [x] table)
          Just facts -> sieve' xs (foldl reinsert (Map.delete x table) facts)
            where
                reinsert table prime =
                    Map.insertWith (++) (x + prime) [prime] table
