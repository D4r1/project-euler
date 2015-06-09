module Euler.E25 where
import Euler.Lib (intLength)

euler25 :: Integer -> Int
euler25 n = fibWalker ((<=) n . intLength)

fibWalker :: (Integer -> Bool) -> Int
fibWalker f = h f 2 0 1
	where
		h :: (Integer -> Bool) -> Int -> Integer -> Integer -> Int
		h f' c n'' n'
			| f' n       = c
			| otherwise = h f' (c+1) n' n
			where n = n' + n''

main :: IO ()
main = print $ euler25 1000
