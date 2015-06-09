module Euler.E30 where

import Euler.Lib (intToList)

euler30 :: Int -> Int
euler30 n = sum $ [ x | x <- [2 .. upperLimit n], isSumOfDigitsPow n x]

upperLimit :: Int -> Int
upperLimit n = aux $ last $ takeWhile (helper) $ dropWhile (not . helper) [1..]
	where
		aux :: Int -> Int
		aux = (* 9^n)
		helper :: Int -> Bool
		helper x = x == (length $ intToList $ aux x)

isSumOfDigitsPow :: Int -> Int -> Bool
isSumOfDigitsPow n x = x == (sum $ map (^n) $ intToList $ x)

main :: IO ()
main = print $ euler30 5
