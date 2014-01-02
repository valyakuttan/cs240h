
import System.Random
import Control.Monad

guess :: (RandomGen g) => (Double,g) -> (Double,g)
guess (_,g) = (z, g'')
    where z        = x^2 + y^2
          (x, g')  = random g
          (y, g'') = random g'

withGen :: (StdGen -> a) -> IO a
withGen f = do
  g <- getStdGen
  let (g',g'') = split g
  setStdGen g'
  return (f g'')

--approxPI :: Int -> IO Double
approxPI n = do
    let guesses = fst `ffmap` withGen (iterate guess . ((,) 0))
    let samples = (tail . take (n+1)) `fmap` guesses
    xs <- samples
    ys <- samples
    let darts =  zipWith sqrsum xs ys
    let x = length . filter (>1) $ darts
    return $ 4 * (fromIntegral (n - x) / fromIntegral n)
  where
      sqrsum x y = x * x + y * y
      ffmap      = fmap . fmap
