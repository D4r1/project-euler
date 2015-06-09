module Euler.E62 where

import Data.List
	( sort
	)
import Data.Map
	( Map
	, empty
	, insertWith
	, notMember
	, (!)
	)
import Euler.Lib
	( intToList
	, listToInt
	, intLength
	)

type CMap = Map Integer [ Integer ]

-- Property: a cube has digital root 1, 8 or 9.
-- Note: the reciprocal is not true.

euler62 :: Int -> Integer
euler62 n = cubeWalker empty n 1 [ 1 .. ]

cubeWalker :: CMap -> Int -> Integer -> [ Integer ] -> Integer
cubeWalker _ _ _    [] = undefined
cubeWalker m n y (x:xs)
	| y' > y            = cubeWalker m'' n y' xs
	| x3' `notMember` m = cubeWalker m'  n y' xs
	| l == n            = x' * x' * x'
	| otherwise         = cubeWalker m'  n y' xs
	where
		x3  = x * x * x
		x3' = listToInt $ sort $ intToList x3
		m'  = insertWith (++) x3' [x] m
		xs' = m' ! x3'
		l   = length $ xs'
		y'  = intLength x3
		m'' = insertWith (++) x3' [x] empty
		x'  = minimum xs'

-- Compute a cube
-- put it in a set
-- if another number is already in the set call GG
-- if the number of digits changes, toss the previous set

main :: IO ()
main = print $ euler62 5
