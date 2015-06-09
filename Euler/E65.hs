module Euler.E65 where

import Euler.Lib
	( intToList
	)

euler65 :: Int-> Integer
euler65 n = sum $ intToList $ fst $ seqToRatio $ take n $ eConvergents

seqToRatio :: [Integer] -> (Integer, Integer)
seqToRatio s = (a * d + n, d)
	where
		(n,d) = seqMerge $ tail s
		a   = head s

seqMerge :: [Integer] -> (Integer,Integer)
seqMerge    []  = undefined
seqMerge (x:[]) = (1,x)
seqMerge (x:xs) = (d , n + x * d)
	where
		(n,d) = seqMerge xs

eConvergents :: [Integer]
eConvergents = 2:(concatMap (\x -> [1, 2*x, 1]) [1..])

main :: IO ()
main = print $ euler65 100
