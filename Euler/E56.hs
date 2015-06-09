module Euler.E56 where

import Euler.Lib (intToList)

euler56 :: Integer -> Integer
euler56 n = maximum $ map (sum . intToList) $ concatMap (pows as) bs
	where
		as = [0..n]
		bs = [0..n]

pows :: [Integer] -> Integer -> [Integer]
pows as b = map (\x-> x^b) as

main :: IO ()
main = print $ euler56 99
