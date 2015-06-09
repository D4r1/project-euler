module Euler.E5 where
import Data.List (isPrefixOf)
import Euler.Lib (divList)

euler5 :: Integer -> Integer
euler5 n = last $ head $ dropWhile (not . ([1..n] `isPrefixOf`)) $ map (cappedDivList n) [n,(2*n)..]

cappedDivList :: Integer -> Integer -> [Integer]
cappedDivList c n = (takeWhile (<= c) (divList n)) ++ [n]

main :: IO ()
main = print $ euler5 20
