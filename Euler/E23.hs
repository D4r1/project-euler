module Euler.E23 where
import qualified Data.Set as S (fold, fromList)
import Euler.Lib (isAbundant)

euler23 :: Int -> Int
euler23 n = sum [1..n] - s
	where
		as = abundantList n
		s = S.fold (+) 0 $ S.fromList $ abundantSumList n as as

abundantList :: Int -> [Int]
abundantList n = filter isAbundant [1..n]

abundantSumList ::  Int -> [Int] -> [Int] -> [Int]
abundantSumList _ [] _ = []
abundantSumList n as []
	| (head as) > n `div` 2 = []
	| otherwise = abundantSumList n (tail as) (tail as)
abundantSumList n as bs
	| i > n = xs
	| otherwise = i:xs
	where
		i = (head as) + (head bs)
		xs = abundantSumList n as (tail bs)

main :: IO ()
main = print $ euler23 28123
