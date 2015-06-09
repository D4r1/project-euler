module Euler.E58 where

import Data.Ratio (Ratio, (%))
import Euler.Lib (isPrime)

euler58 :: Ratio Int -> Int
euler58 r = findRatio r 3 3

len :: Int -> Int
len n = 2 * n - 1

getRatio :: Int -> Ratio Int
getRatio n = (sum $ map primeCount [1,3..n]) % ( len n )

findRatio :: Ratio Int -> Int -> Int -> Int
findRatio r p n
	| r >= p % (len n)  = n
	| otherwise       = findRatio r p' n'
	where
		p' = p + primeCount n'
		n' = n + 2

-- |Assumes n is odd
primeCount :: Int -> Int
primeCount 1 = 0
primeCount n = (length $ filter isPrime $ corners n)

-- |Assumes n is odd
corners :: Integral a => a -> [a]
corners n = [n*n-n-n-n+3, n*n-n-n+2, n*n-n+1, n*n]

main :: IO ()
main = print $ euler58 (10 % 100)
