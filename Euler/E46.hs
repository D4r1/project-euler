module Euler.E46 where

import Data.List (intersect)
import Euler.Lib (composites, primes, squares)

euler46 :: Int
euler46 = head $ dropWhile isGoldbach oddComposites

-- We will assume here that n is an odd composite
isGoldbach :: Int -> Bool
isGoldbach n = not $ null $ intersect ns ps
	where
		ps = takeWhile (<n) primes
		ns = map (\ x -> n - x - x) $
		     takeWhile (< n + n) squares

oddComposites :: [Int]
oddComposites = [ x | x <- composites, odd x ]

main :: IO ()
main = print $ euler46
