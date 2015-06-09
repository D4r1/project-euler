module Euler.E68 where

import Data.Maybe
import Data.List
	( permutations
	, group
	, sort
	)
import Euler.Lib
	( rotateL
	, intToList
	, intConcat
	)

type NGon = [[Int]]

euler68 :: Int -> Int -> Int
euler68 n l = head $ dropWhile (> 10^l) $ reverse $ sort $ map nGonToInt $ genNGons n

nGonToInt :: NGon -> Int
nGonToInt n = foldl1 intConcat ds
	where
		ds = concat n

-- Note: does not work for 6-gons or more (because the original numbers went
-- over 10).
intToNGon :: Int -> NGon
intToNGon n = group3 $ intToList n
	where
		group3 :: [Int] -> [[Int]]
		group3 xs
			| null xs = []
			| otherwise = [take 3 xs] ++ (group3 $ drop 3 xs)

isValidNGon :: NGon -> Bool
isValidNGon n = (length $ group $ map sum n) == 1

listToNGon :: [Int] -> Maybe NGon
listToNGon l
	| isValidNGon n = Just n
	| otherwise     = Nothing
	where
		n  = zipWith3 (\ x y z -> [x,y,z]) os is (rotateL is)
		s  = (length l) `div` 2
		os = take s l -- outsides
		is = drop s l -- insides

genNGons :: Int -> [NGon]
genNGons n = catMaybes $ map listToNGon ls
	where
		ls = filter (isValidPerm n) $ permutations [ 1 .. 2 * n ]

isValidPerm :: Int -> [Int] -> Bool
isValidPerm n p = and
	[ (sum $ take n p) `mod` n == 0
	, (sum $ drop n p) `mod` n == 0
	, (head $ take n p) == (minimum $ take n p)
	]

main :: IO ()
main = print $ euler68 5 16
