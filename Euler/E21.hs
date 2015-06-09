module Euler.E21 where
import qualified Data.Map as M
import Euler.Lib (propDivList)

type PairMap = M.Map Int Bool

euler21 :: Int -> Int
euler21 n = sum $ M.keys $ M.filter id $ foldr checkPair M.empty [1..n]

funD :: Int -> Int
funD = sum . propDivList

checkPair :: Int -> PairMap -> PairMap
checkPair n m
	| n `M.member` m = m
	| otherwise = m''
	where
		n'  = funD n
		n'' = funD n'
		b = and [n==n'', n/=n']
		m' = M.insert n b m
		m''
			| b         = M.insert n' b m'
			| otherwise = m'

main :: IO ()
main = print $ euler21 10000
