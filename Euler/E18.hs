module Euler.E18 where

-- Note: a *much* efficient algorithm is implemented in Project EUler 67

triangle :: [[Int]]
triangle =
	[ [75]
	, [95, 64]
	, [17, 47, 82]
	, [18, 35, 87, 10]
	, [20, 04, 82, 47, 65]
	, [19, 01, 23, 75, 03, 34]
	, [88, 02, 77, 73, 07, 63, 67]
	, [99, 65, 04, 28, 06, 16, 70, 92]
	, [41, 41, 26, 56, 83, 40, 80, 70, 33]
	, [41, 48, 72, 33, 47, 32, 37, 16, 94, 29]
	, [53, 71, 44, 65, 25, 43, 91, 52, 97, 51, 14]
	, [70, 11, 33, 28, 77, 73, 17, 78, 39, 68, 17, 57]
	, [91, 71, 52, 38, 17, 14, 91, 43, 58, 50, 27, 29, 48]
	, [63, 66, 04, 68, 89, 53, 67, 30, 73, 16, 69, 87, 40, 31]
	, [04, 62, 98, 27, 23, 09, 70, 98, 73, 93, 38, 53, 60, 04, 23]
	]

euler18 :: Int
euler18 = maxSumTriangle triangle

maxSumTriangle :: Integral a => [[a]] -> a
maxSumTriangle t
	| null t = 0
	| length t == 1 = maximum (t!!0)
	| otherwise = (t!!0!!0) + max (maxSumTriangle (left t)) (maxSumTriangle (right t))

left :: [[a]] -> [[a]]
left t
	| length t == 0 = []
	| length (t!!0) == 1 = left (tail t)
	| otherwise = (init (t!!0)):(left (tail t))

right :: [[a]] -> [[a]]
right t
	| length t == 0 = []
	| length (t!!0) == 1 = right (tail t)
	| otherwise = (tail (t!!0)):(right (tail t))

main :: IO ()
main = print $ euler18 
