module Euler.E60 where

import Data.List (intersect)
import Data.Map ((!),Map, empty, insert, keys, adjust)
import Euler.Lib (primes, isPrime, intConcat)

type CMap = Map Int [Int]

euler60 :: Int -> Int
euler60 n = sum $ snd $ findCandidates empty n $ primes

isValidPair :: Int -> Int -> Bool
isValidPair x y = and [ isPrime $ intConcat x y, isPrime $ intConcat y x ]

findCandidates :: CMap -> Int -> [Int] -> (CMap, [Int])
findCandidates m _ [] = (m, [])
findCandidates m n (p:ps)
	| null cs        = findCandidates m' n ps
	| length vs == n = (m'', vs)
	| otherwise      = findCandidates m'' n ps
	where
		ks  = potentialMatches m
		cs  = filter (isValidPair p) ks
		m'  = insert p (p:cs) m
		m'' = updateCandidates m' p cs
		vs  = getCycles m'' n (p:cs)

getCycles :: CMap -> Int -> [Int] -> [Int]
getCycles m n xs = filter (not . null . getCycle m n) xs

getCandidates :: CMap -> Int -> [Int] -> [Int]
getCandidates m n xs
	| f == xs   = xs
	| otherwise = getCandidates m n f
	where
		f = filter (\ x -> (>= n) $ length $ (xs) `intersect` (m ! x)) (xs)

getCycle :: CMap -> Int -> Int -> [Int]
getCycle m n x = filter (not . null . getCandidates m n . (m!)) xs
	where
		xs = getCandidates m n (m ! x)

potentialMatches :: CMap -> [Int]
potentialMatches m = keys m

updateCandidates :: CMap -> Int -> [Int] -> CMap
updateCandidates m x cs = foldl (\ acc k -> adjust (x:) k acc) m cs

main :: IO ()
main = print $ euler60 5
