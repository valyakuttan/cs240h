
import Criterion.Main
import Length

main = defaultMain [ bench "len0" $ whnf length0 [1..100000 ] ]
