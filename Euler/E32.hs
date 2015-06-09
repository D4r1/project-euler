module Euler.E32 where

import Data.List (sort, permutations, nub)
import Euler.Lib (intToList, listToInt)

euler32 :: Int
euler32 = sum $ nub $ findPandigitalProduct $ uniqueSizedPermutations 5 $ permutations [1..9]

-- All pandigital products are the product of a numder of two digits and a
-- number of three digits, since the hights two-by-two is 98*76 == 7448, which
-- only gives eight digits total, and the lowest three-by-three is 123*456 ==
-- 56088 whiwh is eleven digits long.
-- EDIT: It looks like some numbers are missing; I forgot to count one-by-four
-- (9*8765=78885, ten digits), which yield the correct result.

uniqueSizedPermutations :: Int -> [[Int]] -> [[Int]]
uniqueSizedPermutations n ps = map (take n) ps

findPandigitalProduct :: [[Int]] -> [Int]
findPandigitalProduct [] = []
findPandigitalProduct (p:ps)
	| and [xy, xy'] = (x  * y ):(x'*y'):xys
	| xy            = (x  * y ):xys
	| xy'           = (x' * y'):xys
	| otherwise     = xys
	where
		(x , p' ) = getNumFromDigits p   2
		(x', p'') = getNumFromDigits p   1
		(y , _  ) = getNumFromDigits p'  3
		(y', _  ) = getNumFromDigits p'' 4
		xy        = isProductPandigital x  y
		xy'       = isProductPandigital x' y'
		xys       = findPandigitalProduct ps

getNumFromDigits :: [Int] -> Int -> (Int,[Int])
getNumFromDigits ds n = (listToInt as, ds')
	where
		(as, ds') = splitAt n ds

isProductPandigital :: Int -> Int -> Bool
isProductPandigital = isProductNPandigital 9

isProductNPandigital :: Int -> Int -> Int -> Bool
isProductNPandigital n x y = (sort $ concatMap intToList $ [x, y, x*y]) == [1..n]

main :: IO ()
main = print $ euler32
