--
module Length where

length0 :: [a] -> Int
length0 [] = 0
length0 (_:xs) = 1 + length0 xs

