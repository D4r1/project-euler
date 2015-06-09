module Euler.E37 where

import Data.List (inits, tails, delete, nub)
import Euler.Lib (intToList, listToInt, isPrime, primes)

euler37 :: Int -> Int
euler37 n = sum $ take n $ filter isTruncatablePrime $ dropWhile (<10) $ primes

isTruncatablePrime :: Int -> Bool
isTruncatablePrime n = and $ map (isPrime . listToInt) $ ns
	where
		ns = delete [] $ nub $ inits n' ++ tails n'
		n' = intToList n

main :: IO ()
main = print $ euler37 11
