module Euler.E24 where
import Data.List (delete)

baseArray :: [Char]
baseArray = "0123456789"

euler24 :: Int -> Int
euler24 n = read $ getPermutation baseArray (n-1)

-- Compute the number of permutations existing for a given set (basically
-- (length n)!).
numPermutations :: [a] -> Int
numPermutations as = product [1..l]
	where l = length as

-- returns the i-th lexicographic permutation of a base array
getPermutation :: Eq a => [a] -> Int -> [a]
getPermutation [] _ = []
getPermutation cs n = c : getPermutation (delete c cs) n
	where
		l = length cs
		p = numPermutations cs
		-- I still don't know how I pulled this one out... but it's the right
		-- index!
		c = cs!!((n*l `div` p) `mod` l)

main :: IO ()
main = print $ euler24 1000000
