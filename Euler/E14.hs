module Euler.E14 where
import Data.Map
import Euler.Lib (collatz)

{- Maps an integer to the length of its Collatz sequence -}
type CollatzMap = Map Int Int

euler14 :: Int -> Int
euler14 n = fst $ foldrWithKey collatzHelper (0,0) $ collatzLength 0 1 empty n

collatzHelper :: Int -> Int -> (Int, Int) -> (Int, Int)
collatzHelper k v acc
	| v > snd acc = (k,v)
	| otherwise = acc

{- Generates a map of the full Collatz based on a starting point.
- If provided with an existing map, it will extend it, saving a lot of
- intermediate computing (lookups instead)
-}
collatzSeq :: Int -> CollatzMap -> (Int, CollatzMap)
collatzSeq n m
	| n == 1 = (1, singleton 1 1)
	| n `member` m = (m ! n, m)
	| otherwise = (l+1, insert n (l+1) m')
	where
		(l, m') = collatzSeq (collatz n) m

collatzLength :: Int -> Int -> CollatzMap -> Int -> CollatzMap
collatzLength mx n cm l
	-- If we exceed the bound, just return the full Collatz Map
	| n > l = cm
	-- If we find a new maximum, sotre the value and keep on searching
	| l' > mx = collatzLength l' (n + 1) m' l
	-- The current subject n is not a maximum; keep on searching
	| otherwise = collatzLength mx (n + 1) m' l
	where
		(l',m') = collatzSeq n cm

main :: IO ()
main = print $ euler14 1000000
