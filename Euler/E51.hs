module Euler.E51 where

import Data.List (sort, nub)
import Euler.Lib (intToList, listToInt, isPrime, primes)

euler51 :: Int -> Int
euler51 n = head $ dropWhile (\x -> (maxSimilarPrimes x) < n) $ primes

maxSimilarPrimes :: Int -> Int
maxSimilarPrimes n = maximum $ map length $ findSimilarPrimes n

-- Assuming n is prime
findSimilarPrimes :: Int -> [[Int]]
findSimilarPrimes n = map (\d -> dropWhile (<n) $ filter isPrime $ replaceDigit n d) ds
	where
		ds = digitList n

digitList :: Int -> [Int]
digitList n = sort $ nub $ intToList n

replaceDigit :: Int -> Int -> [Int]
replaceDigit n d = map (replaceAll n d) [0..9]

replaceAll :: Int -> Int -> Int -> Int
replaceAll n a b = listToInt $ map (sub a b) $ intToList n

sub :: Eq a => a -> a -> a -> a
sub a b x = if x == a then b else x

main :: IO ()
main = print $ euler51 8
