module Euler.E50 where

import Euler.Lib (isPrime, primes)

euler50 :: Int -> Int
euler50 n = last $ head $ dropWhile null $ reverse $ findSumsWithBound n primes

findSumsWithBound :: Int -> [Int] -> [[Int]]
findSumsWithBound n ps = sumMapper n [2..] ps

sumMapper :: Int -> [Int] -> [Int] -> [[Int]]
sumMapper _ [] _ = []
sumMapper n (c:cs) ps 
	| s' > n    = []
	| otherwise = s:ss
	where
		s' = sum $ take c $ ps
		s  = findSumsWithBoundAndCount n c ps
		ss = sumMapper n cs ps

findSumsWithBoundAndCount :: Int -> Int -> [Int] -> [Int]
findSumsWithBoundAndCount _ _ [] = []
findSumsWithBoundAndCount n c ps
	| x > n       = []
	| isPrime $ x = x:xs
	| otherwise   = xs
	where
		x = sum $ take c $ ps
		xs = findSumsWithBoundAndCount n c $ tail ps

main :: IO ()
main = print $ euler50 1000000
