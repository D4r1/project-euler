module Euler.E29 where

import qualified Data.Set as S

euler29 :: Integer -> Int
euler29 n = S.size $ S.fromList [ a^b | a <- [2..n], b <- [2..n] ]

main :: IO ()
main = print $ euler29 100
