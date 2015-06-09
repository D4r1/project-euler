module Euler.E33 where

import Data.Ratio ((%), Ratio, denominator)
import Data.List (intersect, delete)
import Euler.Lib (intToList, listToInt)

euler33 :: Int -> Int
euler33 n = denominator $ product $ findDigitCancellingFraction [10..n] [10..n]

findDigitCancellingFraction :: [Int] -> [Int] -> [Ratio Int]
findDigitCancellingFraction [] _       = []
findDigitCancellingFraction (_:xs') [] = findDigitCancellingFraction xs' xs'
findDigitCancellingFraction xs@(x:_) (y:ys')
	| i x y     = (x % y) : f xs ys'
	| otherwise = f xs ys'
	where
		f = findDigitCancellingFraction
		i = isDigitCancellingFraction

isDigitCancellingFraction :: Int -> Int -> Bool
isDigitCancellingFraction x y
	| x == y    = False -- does not count in the final product, removed for readability
	| null ds   = False
	| y' == 0   = False
	| d  == 0   = False
	| otherwise = x % y == x' % y'
	where
		xs = intToList x
		ys = intToList y
		ds = xs `intersect` ys
		d  = head ds
		x' = listToInt $ delete d xs
		y' = listToInt $ delete d ys

main :: IO ()
main = print $ euler33 99
