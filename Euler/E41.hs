module Euler.E41 where

import Data.List (permutations)
import Euler.Lib (listToInt, isPrime)

euler41 :: Int -> Int
euler41 n = maximum $ filter (isPrime) $ map listToInt $ buildPerms n

buildPerms :: Int -> [[Int]]
buildPerms n = concatMap buildPerm [1..n]

buildPerm :: Int -> [[Int]]
buildPerm n = permutations [1..n]

main :: IO ()
main = print $ euler41 9
