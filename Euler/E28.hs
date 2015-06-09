module Euler.E28 where

euler28 :: Int -> Int
euler28 n = sum $ map (\f -> f n) [neSum, swSum, seSum, nwSum]

neSum :: Int -> Int
neSum n = sum $ [ x*x | x <- [1..n], odd x]
swSum :: Int -> Int
swSum n = sum $ [ x*x+1 | x <- [1..n], even x]
seSum :: Int -> Int
seSum n = sum $ [ x*x-x+1 | x <- [1..n], even x]
nwSum :: Int -> Int
nwSum n = sum $ [ x*x+x+1 | x <- [1..n], even x ]

-- I don't need that anymore to solve this problem; I will just keep it around
-- in case I need it later.
{-
buildSpiral :: Int -> [[Int]]
buildSpiral 1 = [[1]]
buildSpiral n
	| even n = error "Only use with odd sizes."
	| otherwise =
		addTop [tl..tr'] $
		addLeft [tl-1,tl-2.. bl] $
		addBottom [bl-1,bl-2 .. br] $
		addRight [tr .. br-1] $
		buildSpiral (n-2)
		where
			tr  = (n-2)*(n-2)+1
			br  = tr + n - 2
			bl  = br + n - 1
			tl  = bl + n - 1
			tr' = n*n

addTop :: [Int] -> [[Int]] -> [[Int]]
addTop ys xss 
	| length (xss!!0) /= length ys = error "addTop: length do not match."
	| otherwise                    = ys:xss

addBottom :: [Int] -> [[Int]] -> [[Int]]
addBottom ys xss
	| length (xss!!0) /= length ys = error "addBottom: length do not match."
	| otherwise                    = xss ++ [ys]

addLeft :: [Int] -> [[Int]] -> [[Int]]
addLeft ys xss
	| length xss /= length ys = error "addLeft: length do not match."
	| otherwise                    = zipWith f xss ys
	where
		f xs y = y:xs

addRight :: [Int] -> [[Int]] -> [[Int]]
addRight ys xss
	| length xss /= length ys = error "addRight: length do not match."
	| otherwise                    = zipWith f xss ys
	where
		f xs y = xs ++ [y]
-}

main :: IO ()
main = print $ euler28 1001
