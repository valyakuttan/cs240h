-- Stephen's Arrow Tutorial
-- from Haskell wiki books
{-# LANGUAGE Arrows #-}

module Main where

import Control.Arrow
import Control.Monad
import qualified Control.Category as Cat
import Data.List
import Data.Maybe
import System.Random

newtype Circuit a b = Circuit { unCircuit :: a -> (b, Circuit a b) }

instance Cat.Category Circuit where
    id  = Circuit $ \a -> (a, Cat.id)
    (.) = dot

dot :: Circuit b c -> Circuit a b -> Circuit a c
(Circuit c2) `dot` (Circuit c1) = Circuit $ \a ->
    let (b, c1') = c1 a
        (c, c2') = c2 b
        c3       = c2' `dot` c1'
    in (c, c3)

instance Arrow Circuit where
    arr f   = Circuit $ \a -> (f a, arr f) 
    first (Circuit c1)  = Circuit $ \(b,d) ->
        let (c, c1') = c1 b
        in ((c,d), first c1')

runCircuit :: Circuit a b -> [a] -> [b]
runCircuit _    []     = []
runCircuit cir  (x:xs) =
    let (x', cir') = unCircuit cir x
    in x' : runCircuit cir' xs

-- | Accumulator that outputs a value determined by the supplied function.
accum :: acc -> (a -> acc -> (b, acc)) -> Circuit a b
accum acc f = Circuit $ \input ->
    let (output, acc') = input `f` acc
    in  (output, accum acc' f)

-- | Accumulator that outputs the accumulator value.
accum' :: b -> (a -> b -> b) -> Circuit a b
accum' acc f = accum acc (\a b -> let b' = a `f` b in (b', b'))

total :: Num a => Circuit a a
total = accum' 0 (+)

mean1 :: Fractional a => Circuit a a
mean1 = (total &&& (const 1 ^>> total)) >>> arr (uncurry (/))

mean2 :: Fractional a => Circuit a a
mean2 = proc value -> do
    t <- total -< value
    n <- total -< 1
    returnA -< t / n

