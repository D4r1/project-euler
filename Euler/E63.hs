module Euler.E63 where

import Euler.Lib
	( intLength
	)

euler63 :: Int
euler63 = length $ concat $ takeWhile (not . null) $ map validNums [ 1 .. ]

validNums :: Integer -> [Integer]
validNums n = takeWhile ((== n) . intLength) $ dropWhile ((<n) . intLength) $
	[ x^n
	| x <- [ 1 .. ]
	]

main :: IO ()
main = print $ euler63
