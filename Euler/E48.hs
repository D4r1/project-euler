module Euler.E48 where

import Euler.Lib (intToList, listToInt)

euler48 :: Int -> Int -> Integer
euler48 d n = listToInt $ takeLast d $ intToList $ sum $ take n $ [ x^x | x <- [1 .. ] ]

takeLast :: Int -> [a] -> [a]
takeLast n xs = drop (length xs - n) xs

main :: IO ()
main = print $ euler48 10 1000
