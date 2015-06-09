module Euler.E61 where

import Data.List
	( permutations
	)
import Euler.Lib 
	(intConcat
	, isTriangular
	, isSquare
	, isPentagonal
	, isHexagonal
	, isHeptagonal
	, isOctagonal
	)

type Int' = (Int, Int)
type IntList = [ Int' ]
type IntCycle = [ Int' ]

euler61 :: Int
euler61 = sum $ cycleToInts $ head $ makeCycles

cycleToInts :: IntCycle -> [ Int ]
cycleToInts c = zipWith intConcat xs ys
	where
		(xs, ys) = unzip c

makeCycles :: [ IntCycle ]
makeCycles = map head $ filter (not . null) $ map buildCycle $ permutations $ candidates

triangularCandidates :: IntList
triangularCandidates = candidatesFunc isTriangular

squareCandidates :: IntList
squareCandidates = candidatesFunc isSquare

pentagonalCandidates :: IntList
pentagonalCandidates = candidatesFunc isPentagonal

hexagonalCandidates :: IntList
hexagonalCandidates = candidatesFunc isHexagonal

heptagonalCandidates :: IntList
heptagonalCandidates = candidatesFunc isHeptagonal

octagonalCandidates :: IntList
octagonalCandidates = candidatesFunc isOctagonal

candidatesFunc :: (Int -> Bool) -> IntList
candidatesFunc f =
	[ (x, y)
	| x <- [ 10 .. 99 ]
	, y <- [ 10 .. 99 ]
	, f $ intConcat x y
	]

candidates :: [ IntList ]
candidates =
	[ triangularCandidates
	, squareCandidates
	, pentagonalCandidates
	, hexagonalCandidates
	, heptagonalCandidates
	, octagonalCandidates
	]

isCycle :: IntCycle -> Bool
isCycle xs = (fst $ head xs) == (snd $ last xs)

findMatches :: [ IntCycle ] -> IntList -> [ IntCycle ]
findMatches xss ys =
	[ xs ++ [y]
	| xs <- xss
	, y <- ys
	, (snd $ last xs) == (fst y)
	]

buildCycle :: [ IntList ] -> [ IntCycle ]
buildCycle xss = filter isCycle $ foldl findMatches (map (\x -> [x]) $ head xss) (tail xss)

main :: IO ()
main = print $ euler61
