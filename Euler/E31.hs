module Euler.E31 where

coins :: [Coin]
coins = [200, 100, 50, 20, 10, 5, 2, 1]

type Coin = Int
type Amount = Int

-- Tactics:
-- 1: try to add all different amounts of one coin (without going over the
-- limit), and count how many combination exists with the remaining coins.
-- 2: recurse
-- 3: ???
-- 4: profit
numWallets :: Amount -> [Coin] -> Int
numWallets a (c:cs) = sum $ map (\n -> numWallets (a - n*c) cs) [0 .. (a `div` c)]
numWallets a []
	| a == 0    = 1
	| otherwise = 0

euler31 :: Amount -> Int
euler31 n = numWallets n coins

main :: IO ()
main = print $ euler31 200
