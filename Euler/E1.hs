module Euler.E1 where
euler1 :: Int -> Int
euler1 n = sum [ x | x <- [1..(n-1)], or [x `mod` 3 == 0, x `mod` 5 == 0]]

main :: IO ()
main = print $ euler1 1000
