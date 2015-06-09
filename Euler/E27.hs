module Euler.E27 where

import Data.List
import Data.Function
import Euler.Lib (isPrime)

type PerfMap = [ ( Int, Integer ) ]

euler27 :: Integer -> Integer
euler27 n = snd $ maximumBy (compare `on` fst) $ walker $ n

walker :: Integer -> PerfMap
walker n = [ (p x y, (x*y)) | x <- bounds, y <- bounds, isPrime $ abs x, isPrime $ abs y ]
	where
		p x' y' = producedPrimes $ quadratic x' y'
		bounds = [(-n) .. n]

producedPrimes :: (Integer -> Integer) -> Int
producedPrimes f = length $ takeWhile (\x -> and [f x > 0, isPrime $ f x]) $ [0..]

quadratic :: Integer -> Integer -> Integer -> Integer
quadratic a b n = n*n + a*n + b

main :: IO ()
main = print $ euler27 1000
