
import Control.Monad.ST (ST)
import qualified Data.Vector.Unboxed.Mutable as V
import qualified Data.Vector.Unboxed as U

vsort :: U.Vector Int -> U.Vector Int
vsort v = U.create $ do
            vec <- U.thaw v
            quicksort vec
            return vec

quicksort :: V.MVector s Int -> ST s ()
quicksort v = recur 0 $ V.length v
  where 
      recur left right
        | left >= right = return ()
        | otherwise = do
        idx <- partition left right
               (left + (right - left) `div` 2)
        recur left (idx - 1)
        recur (idx + 1) right

      partition left right pivotIndex = do
        pivot <- V.read v pivotIndex
        V.swap v pivotIndex right
        let loop i k
              | i == right = V.swap v k right >>
                             return k
              | otherwise = do
              x <- V.read v i
              if x < pivot
                then V.swap v i k >>
                     loop (i + 1) (k + 1)
                else loop (i + 1) k
        loop left right
