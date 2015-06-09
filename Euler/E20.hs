module Euler.E20 where
import Data.Char

euler20 :: Integer -> Int
euler20 n = sum $ map digitToInt $ show $ product [1..n]

main :: IO ()
main = print $ euler20 100
