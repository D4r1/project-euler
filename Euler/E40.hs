module Euler.E40 where

import Euler.Lib (champerowne)

euler40 :: Int -> Int
euler40 n = product $ map (\ x -> champerowne !! (10 ^ x)) [0..n]

main :: IO ()
main = print $ euler40 6
