module Euler.E34 where

import Euler.Lib (fact, intToList)

euler34 :: Integer
euler34 = sum $ [ x | x <- [10..7* fact 9], isCuriousNumber x]
-- Note: the upper bound is 7*9! because next possible numbers only have seven
-- digits (meaning no possible match, as if all of the digits were nines, it
-- would not reach the number's value).

isCuriousNumber :: Integer -> Bool
isCuriousNumber n = n == (sum $ map fact $ intToList n)

main :: IO ()
main = print $ euler34
