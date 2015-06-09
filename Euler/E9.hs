module Euler.E9 where
euler9 :: Int -> Int
euler9 n = x*y*z
	where
		(x,y,z) = findTriple n

genTriples :: Int -> [(Int, Int, Int)]
genTriples n = [(x,y,z) | x <- [1..n], y <- [x..n], z <- [y..n], x+y+z == n]

isPythTriple :: (Int,Int,Int) -> Bool
isPythTriple (x,y,z) = or
	[ x*x + y*y == z*z
	, x*x + z*z == y*y
	, y*y + z*z == x*x
	]

findTriple :: Int -> (Int,Int,Int)
findTriple n = head $ filter isPythTriple $ genTriples n

main :: IO ()
main = print $ euler9 1000
