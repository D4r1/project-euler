module Euler.E66 where

import Data.Maybe
	( fromJust
	)
import Data.List
	( elemIndex
	)
import Euler.Lib
	( isSquare
	)

euler66 :: Integer -> Int
euler66 n = 1 + (fromJust $ elemIndex x xs)
	where
		x = maximum xs
		xs = map findSolution [ 1 .. n ]

-- Solving the diophantine equation x²-Dy² = 1 with the smallest integers is
-- equivalent to finding the convergents of sqrt(D), at rank D if D is even, and
-- rank 2D+1 if D is odd.
-- (https://en.wikipedia.org/wiki/Pell%27s_equation)
-- (http://mathworld.wolfram.com/PellEquation.html)

data SeqStep = SeqStep
	{ ai :: Integer -- current root
	, ni :: Integer -- current "numerator"
	, di :: Integer -- current denominator
	} deriving (Eq, Show)
-- form: ai + (sqrt n - ni) / di

sqrtSeq :: Integer -> [Integer]
sqrtSeq n 
	| a0 * a0 == n = [head as]
	| otherwise    = (head as):(cycle $ tail as)
	where
		doSeq = seqBuild n
		as =
			map ai $ s2:s3:ss
		a0 = ai s2
		s0 = SeqStep {ai=0, ni=0, di=1} -- base state, always true (direct representation of the square root)
		s1 = seqBuild n s0 -- not used (basically  0 + (sqrt(n)-0)/n)
		s2 = seqBuild n s1 -- first iteration, contains the integer part
		s3 = seqBuild n s2 -- second iteration, used to detect periods
		ss = takeWhile (/=s3) $ tail $ iterate doSeq s3 -- all subsequent decimals

seqBuild :: Integer -> SeqStep -> SeqStep
seqBuild n si = si'
	where 
		si' = SeqStep {ai=ai', ni=ni', di=di'}
		ai' = floor $ (fromIntegral $ di si) / ((sqrt $ fromIntegral n :: Double) - (fromIntegral $ ni si))
		ni' = di' * ai' - ni si
		di' = (n - (ni si) * (ni si)) `div` (di si)

seqToRatio :: [Integer] -> (Integer, Integer)
seqToRatio s = (a * d + n, d)
	where
		(n,d) = seqMerge $ tail s
		a   = head s

seqToRatios :: [Integer] -> [(Integer, Integer)]
seqToRatios s = map (\n  -> seqToRatio $ take n s) [ 1 .. ]
-- No problem with an infinite list, since the source list is often infinite,
-- and we will find the solution somewhere anyway (squares are removed before in
-- the process, and they are the only ones that would generate an infinite
-- list).

seqMerge :: [Integer] -> (Integer,Integer)
seqMerge    []  = (0,1)
seqMerge (x:[]) = (1,x)
seqMerge (x:xs) = (d , n + x * d)
	where
		(n,d) = seqMerge xs

isValidPair :: Integer -> (Integer,Integer) -> Bool
isValidPair n (x,y) = x*x-n*y*y == 1

findSolution :: Integer -> Integer
findSolution n
	| isSquare n = 0
	| otherwise  = fst $ head $ dropWhile (not . isValidPair n) $ seqToRatios $ sqrtSeq n

main :: IO ()
main = print $ euler66 1000
