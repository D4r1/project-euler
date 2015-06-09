module Euler.E39 where

import Data.Function (on)
import Data.List (maximumBy)

euler39 :: Int -> Int
euler39 n = maximumBy (compare `on` numTriangles) [1..n]

numTriangles :: Int -> Int
numTriangles = length . findTriangles

findTriangles :: Int -> [ (Int, Int, Int) ]
findTriangles n =
	[ (x, y, (n - y - x)) |
		x <- [1..n],
		y <- [x..n],
		isValidRightTriangle x y (n - y - x) -- NYXNYXNYXNYX
	]

isValidRightTriangle :: Int -> Int -> Int -> Bool
isValidRightTriangle x y z
	| x * x + y * y == z * z = True
	| otherwise              = False

main :: IO ()
main = print $ euler39 1000
