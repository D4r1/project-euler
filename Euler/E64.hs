module Euler.E64 where

import Data.List
	( intersperse
	)

data SqrtSeq = SqrtSeq { root :: Int, seqA :: [Int] }
data SeqStep = SeqStep
	{ ai :: Int -- current root
	, ni :: Int -- current "numerator"
	, di :: Int -- current denominator
	} deriving (Eq, Show)
-- form: ai + (sqrt n - ni) / di

instance Show SqrtSeq where
	show s =
		"[" ++
		(show $ root s) ++
		";(" ++
		(intersperse ',' $ concatMap show $ seqA s) ++
		")], period=" ++
		(show $ period s)

showStep :: Int -> SeqStep -> String
showStep n s =
		(show $ ai s) ++
		"+ (sqrt(" ++
		show n ++
		") - " ++
		(show $ ni s) ++
		") / " ++
		(show $ di s)

euler64 :: Int -> Int
euler64 n = length $ filter (odd . period . sqrtSeq) $ [ 2 .. n ]

seqToFloat :: SqrtSeq -> Double
seqToFloat s = (fromIntegral a) + foldr seqMerge (fromIntegral $ last as') (init as')
	where
		a   = root s
		as  = seqA s
		as' = take 30 $ cycle as

seqMerge :: Int -> Double -> Double
seqMerge x acc = 1/((fromIntegral x) + acc)

sqrtSeq :: Int -> SqrtSeq
sqrtSeq n 
	| a0 * a0 == n = SqrtSeq
		{ root = head as
		, seqA = [] }
	| otherwise    = SqrtSeq
		{ root = head as
		, seqA = tail as }
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

seqBuild :: Int -> SeqStep -> SeqStep
seqBuild n si = si'
	where 
		si' = SeqStep {ai=ai', ni=ni', di=di'}
		ai' = floor $ (fromIntegral $ di si) / ((sqrt $ fromIntegral n :: Double) - (fromIntegral $ ni si))
		ni' = di' * ai' - ni si
		di' = (n - (ni si) * (ni si)) `div` (di si)

period :: SqrtSeq -> Int
period s = length $ seqA s

main :: IO ()
main = print $ euler64 10000
