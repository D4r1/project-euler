module Euler.E43 where

import Data.List (permutations)
import Euler.Lib (listToInt)

euler43 :: Int
euler43 = sum $ map listToInt $ filter isGud panNums

isGud :: [Int] -> Bool
isGud n = and
	[ d n [2,3,4]  2
	, d n [3,4,5]  3
	, d n [4,5,6]  5
	, d n [5,6,7]  7
	, d n [6,7,8]  11
	, d n [7,8,9]  13
	, d n [8,9,10] 17
	]

subNUm :: [Int] -> [Int] -> Int
subNUm ds os = listToInt $ map (\ x -> ds !! (x-1)) os

d :: [Int] -> [Int] -> Int -> Bool
d ds os n = ((subNUm ds os) `mod` n) == 0

panNums :: [[Int]]
panNums = permutations [0..9]

main :: IO ()
main = print $ euler43
