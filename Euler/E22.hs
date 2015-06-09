module Euler.E22 where
import Data.List
import Data.Char

euler22 :: [String] -> Int
euler22 = snd . foldl nameVal (1,0) . sort

nameVal :: (Int, Int) -> String -> (Int, Int)
nameVal (p,v) n = (p+1, v + p * sum (map (\ c -> 1 + (ord c) - (ord 'A')) n))

main :: IO ()
main = interact $ show . euler22 . lines
