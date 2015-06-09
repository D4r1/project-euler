module Euler.E26 where
import Data.List
import Data.Function

-- with help from
-- http://huangyun.wikispaces.com/Project+Euler+Solution+26

euler26 :: Integer -> Integer
euler26 n = maximumBy (compare `on` period) [1..n]

period :: Integer -> Integer
period n
	| even n         = 0
	| n `mod` 5 == 0 = 0
	| otherwise      = head $ dropWhile (\ x -> (10^x-1) `mod` n /=0) $ [1..]

main :: IO ()
main = print $ euler26 1000
