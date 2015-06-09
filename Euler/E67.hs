module Euler.E67 where

euler67 :: [[Int]] -> Int
euler67 t = head $ foldr triangleFolder (last t) $ init t

triangleFolder :: [Int] -> [Int] -> [Int]
triangleFolder xs acc = zipWith max ls rs
	where
		ls = zipWith (+) xs $ init acc --lefts
		rs = zipWith (+) xs $ tail acc --rights

main :: IO ()
main = interact $ show . euler67 . map read . lines
