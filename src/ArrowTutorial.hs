--
--
{-# LANGUAGE Arrows #-}

module ArrowTutorial where

import Control.Arrow
import Control.Category
import Prelude hiding (id, (.))

newtype SF a b = SF { runF :: a -> b }

newtype Kleisli m a b = Kleisli {
  runKleisli :: (a -> m b) 
}

instance Category SF where
    id = SF id
    (SF g) . (SF f) = SF $ g . f

instance Arrow SF where
    arr = SF
    first (SF f) = SF $ \(b,d) -> (f b, d)
    second (SF f) = SF $ \(d,b) -> (d, f b)

split :: Arrow a => a b (b, b)
split = arr $ \b -> (b, b)

unsplit :: Arrow a => (b -> c -> d) -> a (b, c) d
unsplit = arr . uncurry

liftA2 :: Arrow a => (b -> c -> d) -> a e b -> a e c -> a e d
liftA2 op f g = f &&& g >>> unsplit op

h' :: SF Int Int
h' = proc x -> do
       fx <- arr (*3) -< x
       gx <- arr ((+6) . (*8)) -< x
       returnA -< fx + gx

