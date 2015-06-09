module Euler.E19 where
import Euler.Lib (leapDay, monthLengths)

data Day = Sunday | Monday | Tuesday | Wednesday | Thursday | Friday | Saturday
	deriving (Show, Read, Eq, Ord, Enum)
data Date = Date Int Int Int

euler19 :: Int -> Int
euler19 n = length $ filter (==Sunday) $ [ day (Date x y 1) | x <- [1901..n], y <- [1..12] ]

day :: Date -> Day
day d
	| d' == 0   = Sunday
	| d' == 1   = Monday
	| d' == 2   = Tuesday
	| d' == 3   = Wednesday
	| d' == 4   = Thursday
	| d' == 5   = Friday
	| d' == 6   = Saturday
	| otherwise = error "Day index out of range"
	where
		-- 1900-01-01 was a Monday; hence the "+1"
		d' = (1 + dayToRef d) `mod` 7

dayToRef :: Date -> Int
dayToRef (Date y m d) = y' + m' + (d-1)
	where
		y' = (y - 1900) * 365 + l
		l  = foldr (\x -> (+) (leapDay x)) 0 [ 1900 .. (y-1) ]
		m' = sum $ map (\x -> monthLengths y !! (x-2)) [ 2 .. m ]

main :: IO ()
main = print $ euler19 2000
