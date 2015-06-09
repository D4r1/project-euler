module Euler.E4 where
import Control.Applicative ((<$>),(<*>))
import Data.List (sort)
import Euler.Lib (isPalindrome)

euler4 :: Integer -> Integer
euler4 n = head $ dropWhile (not . isPalindrome) $ reverse $ sort $ fullList n

fullList :: Integer -> [Integer]
fullList n = (*) <$> xs <*> xs
	where
		x = 10 ^ n
		xs = [x,(x-1)..1]

main :: IO ()
main = print $ euler4 3
