module Euler.E36 where

import Euler.Lib (isPalindromeWithBase, isPalindrome)

euler36 :: Int -> Int
euler36 n = sum [ x | x <- [1..n], isPalindrome x, isPalindromeWithBase 2 x]

main :: IO ()
main = print $ euler36 1000000
