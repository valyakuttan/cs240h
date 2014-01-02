--
--
import Control.Monad.Cont

data Target

isTargetValid = undefined
sayUhOh = undefined

data Hole = Swing Int | Attack Target

unitAttack :: Target -> ContT () IO Hole
unitAttack target = ContT $ \k -> do
    k (Swing 60)
    valid <- isTargetValid target
    if valid
    then k (Attack target)
    else sayUhOh

damage    :: Target -> IO ()
damage = undefined

swingBack :: Int -> IO ()
swingBack = undefined

continue :: Hole -> IO ()
continue (Swing  n) = swingBack n
continue (Attack t) = damage t

target = undefined :: Target

main = runContT (unitAttack target) continue :: IO ()

test :: Cont Int Int
test = do
  a <- return 1
  b <- return 5
  return $ a + b
