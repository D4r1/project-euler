module Euler.E2 where
import Euler.Lib (fibs)

euler2 :: Integer -> Integer
euler2 n = sum $ fibList n

fibHelper :: [Integer] -> [Integer]
fibHelper (f:fs) = f : (fibHelper $ drop 2 fs)
fibHelper [] = []


fibList :: Integer -> [Integer]
fibList l = takeWhile (<l) $ fibHelper $ fibs

main :: IO ()
main = print $ euler2 4000000
