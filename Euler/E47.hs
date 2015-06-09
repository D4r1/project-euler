module Euler.E47 where

import Data.List (nub, isPrefixOf)
import Euler.Lib (primeFactors)

euler47 :: Int -> Int
euler47 n = findSeq n $ validNums n

findSeq :: Int -> [Int] -> Int
findSeq _        []  = 0
findSeq n xs'@(x:xs)
	| ys `isPrefixOf` xs' = x
	| otherwise           = findSeq n xs
	where ys = [ x .. x + n - 1 ]
		
validNums :: Int -> [Int]
validNums n = filter (hasNFactors n) $ [ 1 .. ]

hasNFactors :: Int -> Int -> Bool
hasNFactors n x = n == (length $ nub $ primeFactors x)

main :: IO ()
main = print $ euler47 4
