module Euler.E10 where

import Euler.Lib (primes)

euler10 :: Integer -> Integer
euler10 n = sum $ takeWhile (<n) $ primes

main :: IO ()
main = print $ euler10 2000000
