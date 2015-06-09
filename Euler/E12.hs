module Euler.E12 where
import Euler.Lib (divList)

euler12 :: Int -> Int
euler12 n = searchTriangle n 1 2

searchTriangle :: Int -> Int -> Int -> Int
searchTriangle l n c
	| (length . divList) n >= l = n
	| otherwise                 = searchTriangle l (n + c) (c + 1)

main :: IO ()
main = print $ euler12 500
