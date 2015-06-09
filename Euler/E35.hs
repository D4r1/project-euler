module Euler.E35 where

import Data.List ((\\))
import Euler.Lib (rotations, isPrime, intToList, listToInt)

euler35 :: Int -> Int
euler35 n = circularPrimes [2..n]

circularPrimes :: [Int] -> Int
circularPrimes [] = 0
circularPrimes (n:ns)
	| (not $ isPrime n)      = circularPrimes ns
	| (and $ map isPrime rs) = (length rs) + circularPrimes ns'
	| otherwise              = circularPrimes ns
	where
		rs  = map listToInt $ rotations $ intToList n
		ns' = ns \\ rs

main :: IO ()
main = print $ euler35 1000000
