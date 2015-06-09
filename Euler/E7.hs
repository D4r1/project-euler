module Euler.E7 where
import Euler.Lib (primes)

euler7 :: Int -> Int
euler7 n = last $ take n $ primes

main :: IO ()
main = print $ euler7 10001
