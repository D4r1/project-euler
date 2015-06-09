module Euler.E57 where

import Data.Ratio (Ratio, (%), numerator, denominator)
import Euler.Lib (intLength)

euler57 :: Int -> Int
euler57 n = length $ filter isValid $ take n $ approxRoot2

isValid :: Ratio Integer -> Bool
isValid r = (intLength $ numerator r) > (intLength $ denominator r)

approxRoot2 :: [Ratio Integer]
approxRoot2 = map (1+) $ iterate recRoot2 $ 1 % 2

recRoot2 :: Ratio Integer -> Ratio Integer
recRoot2 r = 1 / (2 + r)

main :: IO ()
main = print $ euler57 1000
