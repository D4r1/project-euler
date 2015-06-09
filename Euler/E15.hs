module Euler.E15 where

import Data.Map (Map,empty,insert,(!),member)
{- Important thing to remember here: no memoization implies no performances; the
- thunks are evaluated and garbage collected for each computation, meaning the
- results are lost. The map is here to help us keep track of the already
- computed value to save a lot of computations (we even compute only half of
- them due to symmetry).
-}

type PMap = Map (Int,Int) Integer

euler15 :: Int -> Integer
euler15 n = numPaths (n,n)

numPaths :: (Int, Int) -> Integer
numPaths (x,y) = fst $ pathHelper (x,y) empty

pathHelper :: (Int, Int) -> PMap -> (Integer, PMap)
pathHelper c@(x,y) m
	| x == 0 = (1, insert c 1 m)
	| x > y = (hInv, insert c hInv mInv)
	| c `member` m = (m!c, m)
	| otherwise = (l, insert c l m'')
	where
		(hInv,mInv) = pathHelper (y,x) m
		(h1,m') = pathHelper (x-1,y) m
		(h2,m'') = pathHelper (x,y-1) m'
		l = h1+h2

main :: IO ()
main = print $ euler15 20
