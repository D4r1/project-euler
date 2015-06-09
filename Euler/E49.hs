module Euler.E49 where

import Data.List (permutations, (\\), intersect, delete)
import Euler.Lib (primes, listToInt, intToList)

euler49 :: Int -> Int
euler49 n = resultToInt r
	where
		rs = candidates $ nLongPrimes n
		r  = head $ delete [1487, 4817, 8147] $ rs

resultToInt :: [Int] -> Int
resultToInt xs = listToInt $ concatMap intToList xs

candidates :: [Int] -> [[Int]]
candidates [] = []
candidates xs'@(x:xs)
	| null rs   = candidates xs
	| otherwise = rs:candidates (xs \\ rs)
	where
		rs = step1 $ primePermutations x xs'

nLongPrimes :: Int -> [Int]
nLongPrimes n = takeWhile ( < 10^n ) $ dropWhile ( < 10 ^ (n - 1) ) $ primes

primePermutations :: Int -> [Int] -> [Int]
primePermutations n xs = intersect xs $ map listToInt $ permutations $ intToList n

step1 :: [Int] -> [Int]
step1    [] = []
step1 (x:xs)
	| s == 0    = step1 xs
	| otherwise = [x, x + s, x + 2 * s]
	where
		s = step2 $ map (\ y -> y - x) xs

step2 :: [Int] -> Int
step2    [] = 0
step2 (x:xs)
	| (2*x) `elem` xs = x
	| otherwise       = step2 xs

main :: IO ()
main = print $ euler49 4
