module Euler.E70 where

import Data.List
	( sort
	)
import Euler.Lib
	( totient
	, intToList
	, primes
	)

-- Note: it looks like we have the best values of the totient for semiprimes
-- (i.e. the product of two primes), as phi(p*q)=phi(p)*phi(q) if they are
-- different, and phi(p^2)=p*(p-1) otherwise
-- Tactics: compute the full list of semiprimes (since we have an efficient
-- prime list) and test only those.
-- Updated tactics: he computing of the products of all semiprimes is far too
-- long for 60 seconds; we will instead take a set of all integers and remove
-- all multiples of semiprimes
-- Herm, maybe the value is highest for primes?
-- Okay, so this is the highest for a prime (phi(p) = p-1), but we need the
-- permutations anyway.
-- When limited to 1,000,000, we get 783,169, with totient 781,396, which is the
-- product of 827 and 947 (i.e. a semiprime).
-- We know that phi(s) = (p-1)-q-1), for s = p*q, p and q primes.
-- We can perhaps compute the totient for semiprimes based on close primes?

euler70 :: Int -> Int
euler70 n = snd $ last $ getPerms 5 $ takeWhile (<n) $ primes -- [ 1 .. n ]
-- initial value is five, as it appears to be easy to best (see Wikipedia
-- article "Euler's totient function").

semiPrimeThreshold :: Double
semiPrimeThreshold = 0.8

genSemiPrimes :: Int -> [Int]
genSemiPrimes b = ps
	where
		ps = takeWhile (< floor ( (sqrt $ fromIntegral b) / semiPrimeThreshold)) $ primes

getPerms :: Double -> [Int] -> [(Double, Int)]
getPerms _ [] = []
getPerms r (x:xs)
	-- TODO: we need to not compute the totients as much as possible
	-- | r' > r      = os -- if we exceed the stored ratio, do not bother to check
	| isValid x t = (r', x) : os'
	| otherwise   = os
	where
		t   = x-1
		--t   = totient x
		r'  = q x t
		os  = getPerms r  xs
		os' = getPerms r' xs

isValid :: Int -> Int -> Bool
isValid x t = (sort $ intToList x) == (sort $ intToList t)

q :: Int -> Int -> Double
q n t = (fromIntegral n) / (fromIntegral t)

main :: IO ()
main = print $ euler70 10000000
