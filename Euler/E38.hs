module Euler.E38 where

import Euler.Lib (intToList, isPanDigital, listToInt, intLength)

euler38 :: Int
euler38 = maximum $ concatMap getProds $ [1..50000]

getProds :: Int -> [Int]
getProds n = filter isPanDigital $ map (\ m ->concatenatedProduct n [1..m]) [2..(9 `div` l)]
	where
		l = intLength n

concatenatedProduct :: Int -> [Int] -> Int
concatenatedProduct n xs = listToInt $ concatMap (intToList . (n*)) xs

main :: IO ()
main = print $ euler38
