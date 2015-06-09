module Euler.E45 where

import Euler.Lib (hexagonals, isPentagonal, isTriangular)

euler45 :: Int -> Int
euler45 n = head $
	[ x
	| x <- (dropWhile (<= n) hexagonals)
	, isPentagonal x
	, isTriangular x
	]

main :: IO ()
main = print $ euler45 40755
