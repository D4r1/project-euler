module Euler.E44 where

import Euler.Lib (pentagonals, isPentagonal)

euler44 :: Int
euler44 = abs $ x-y
	where
		(x,y) = head $ findPentagons [] pentagonals

findPentagons :: [Int] -> [Int] -> [(Int,Int)]
findPentagons  _    []  = []
findPentagons ys (x:xs) =
	(filter isPentagonalPair $ map (\ y -> (x,y)) ys) ++
	findPentagons (x:ys) xs

-- We will be assuming that x and y are already pentagonal
isPentagonalPair :: (Int, Int) -> Bool
isPentagonalPair (x, y) = and
	[ isPentagonal $ x + y
	, isPentagonal $ abs $ x - y
	]

main :: IO ()
main = print $ euler44
