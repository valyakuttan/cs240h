--

import Control.Applicative ((<$>))
import Data.Functor.Contravariant (Contravariant (..))
import Data.Function

newtype Predicate a = Predicate { getPredicate :: a -> Bool }

----
---- Contravariant Functors
----
instance Contravariant Predicate where
    contramap g (Predicate p) = Predicate (p . g)

newtype Const a b = Const a

instance Contravariant (Const a) where
    contramap _ (Const a) = Const a

newtype Comparison a = Comparison (a -> a -> Ordering) -- e.g. compare

instance Contravariant Comparison where
    contramap g (Comparison comp) = Comparison (comp `on` g)

newtype Op b a = Op (a -> b)

instance Contravariant (Op b) where
    contramap g (Op f) = Op (f . g)

veryOdd :: Predicate Integer
veryOdd = contramap (`div` 2) (Predicate odd)

main :: IO ()
main = print $ getPredicate veryOdd <$> [0 .. 11]
