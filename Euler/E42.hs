module Euler.E42 where

import Euler.Lib (letterToInt, isTriangular)

euler42 :: [String] -> Int
euler42 ws = length $ filter isTriangular $ map wordToInt ws

wordToInt :: String -> Int
wordToInt w = sum $ map letterToInt w

main :: IO ()
main = interact $ show . euler42 . lines
