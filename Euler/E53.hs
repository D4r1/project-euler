module Euler.E53 where

import Euler.Lib (comb)

euler53 :: Integer -> Integer -> Int
euler53 n l = length $ filter (>l) $ concatMap combs [1..n]

combs :: Integer -> [Integer]
combs n = map (\r -> comb r n) [0..n]

main :: IO ()
main = print $ euler53 100 1000000
