module Euler.E3 where
import Euler.Lib (primeFactors)

euler3 :: Int -> Int
euler3 = last . primeFactors

main :: IO ()
main = print $ euler3 600851475143
