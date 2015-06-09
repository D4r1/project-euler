module Euler.E52 where

import Data.List (sort)
import Euler.Lib (intToList)

euler52 :: Int -> Int
euler52 n = head $ dropWhile (\x -> not $ hasPermutedMultiples n x) [1..]

hasPermutedMultiples :: Int -> Int -> Bool
hasPermutedMultiples n x = and $ map (isPermuted x) xs
	where
		xs = multiples n x

multiples :: Int -> Int -> [Int]
multiples n x = map (x*) [1..n]

isPermuted :: Int -> Int -> Bool
isPermuted x y = xs == ys
	where
		xs = sort $ intToList x
		ys = sort $ intToList y

main :: IO ()
main = print $ euler52 6
