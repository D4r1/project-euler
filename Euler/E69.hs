module Euler.E69 where

import Euler.Lib
	( totient
	, areCoprimes
	)

euler69 :: Int -> Int
euler69 n = snd $ maximum $ [ (q x $ totient x, x) | x <- [ 2 .. n ] ]

q :: Int -> Int -> Double
q n t = (fromIntegral n) / (fromIntegral t)

-- This is the *real full* definition of the totient, not very efficient, though
totient' :: Int -> Int
totient' n = length $ filter (areCoprimes n) $ [ 1 .. (n-1) ]

main :: IO ()
main = print $ euler69 1000000
