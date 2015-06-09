module Euler.E55 where

import Euler.Lib (lychrels)

euler55 :: Int -> Int
euler55 n = length $ takeWhile (<n) lychrels

main :: IO ()
main = print $ euler55 10000
