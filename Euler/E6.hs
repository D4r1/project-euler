module Euler.E6 where
euler6 :: Integer -> Integer
euler6 n = squareSum n - sumSquares n

sumSquares :: Integer -> Integer
sumSquares n = sum [ x*x | x <- [1..n]]

squareSum :: Integer -> Integer
squareSum n = x*x
	where x = sum [1..n]

main :: IO ()
main = print $ euler6 100
