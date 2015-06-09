module Euler.E16 where
import Data.Char

base :: Integer
base = 2

euler16 :: Int -> Int
euler16 n = sum $ map digitToInt $ show $ base^n

main :: IO ()
main = print $ euler16 1000
