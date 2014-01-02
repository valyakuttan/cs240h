
{-# LANGUAGE GADTs, KindSignatures #-}

import qualified Data.Map as Map

--data Maybe1 a where
--    Nothing :: Maybe1 a
--    Just    :: a -> Maybe1 a

test :: Ord k => Map.Map k Int -> k -> k -> Int
test m i j | Just x <- Map.lookup i m
           , Just y <- Map.lookup j m = x
           | _ <- Map.lookup i m
           , _ <- Map.lookup j m = 0

