{-|
	Useful functions used to solve Project Euler problems
|-}

module Euler.Lib where

{-[ Imports ]-}

import Data.List
	( sort
	, group
	)
import Data.Char
	( ord
	, isAsciiLower
	, isAsciiUpper
	)
import Data.Maybe 
	( isNothing
	, fromJust
	)

{-[ Divisors ]-}

-- |Returns the list of divisors of an integer.
divList :: Integral a => a -> [a]
divList n = divisor n 1

-- |Returns the list of the proper divisors of an integer (that is, all divisors
-- except the number itself).
propDivList :: Integral a => a -> [a]
propDivList n = 1:divisor n 2

-- |Small helper function returning the list of divisors starting at a given
-- point; note it only computes the lowest divisors and infers their dual part
-- to save computations.
divisor :: Integral a => a -> a -> [a]
divisor n d
	| n < 0          = map (0-) $ divisor (-n) d
	| d * d > n      = []
	| d * d == n     = [d]
	| n `mod` d == 0 = d:(divisor n (d+1)) ++ [n `div` d]
	| otherwise      = divisor n (d+1)

-- |Tells is a number is abundant or not (i.e. if the some of its proper
-- divisors is greater than the number).
isAbundant :: Integral a => a -> Bool
isAbundant n = sum (propDivList n) > n

-- |Returns the list of the prime factors of a given integer.
primeFactors :: Integral a => a -> [a]
primeFactors n
	| n == 1    = [1]
	| otherwise = factorHelper n 2 b
	where
		b = floor $ sqrt $ ( fromIntegral n :: Double )

-- |Helper function to compute the list of prime divisors of a given integer.
factorHelper :: Integral a => a -> a -> a -> [a]
factorHelper n a b 
	| n == 1    = []
	| null xs   = [n]
	| otherwise = a':(factorHelper (n `div` a') a' b)
	where
		a' = head xs
		xs = [ x | x <- [ a .. b ], n `mod` x == 0 ]

-- Returns all decimals of the euclidian division between two numbers; the first
-- cell of the list holds the integer part and each successive element is
-- another decimal.
euclidDivide :: Integral a => a -> a -> [a]
euclidDivide n d
	| n `mod` d == 0 = [n `div` d]
	| otherwise      = (n `div` d) : euclidDivide (10 * (n `mod` d)) d

-- |Builds the list of all primes, based on the list of the first existing
-- primes.
primesWithList :: (Integral a) => a -> [a] -> [a]
primesWithList _ [] = 2  : ( primesWithList 2  [2] )
primesWithList p ps = p' : ( primesWithList p' ps' )
	where
		p'   = nextPrimeWithList ps p
		ps' = ps ++ [p]

-- |Finds the next prime based on the list of the first primes (increasing
-- order)
nextPrimeWithList :: Integral a => [a] -> a -> a
nextPrimeWithList ps l = head [ x | x <- [ (l+1) .. ], isPrimeWithList ps x]

-- |Small helper function computing if a number is a prime, based on a list of
-- known primes.
isPrimeWithList :: Integral a => [a] -> a -> Bool
isPrimeWithList ps n = and $ map (\ x -> (n `mod` x /= 0)) $ takeWhile (<=b) ps
	where
		b = floor $ (sqrt $ fromIntegral n :: Double)

-- |Checks if a number is prime or not (not very efficient)
isPrime :: Integral a => a -> Bool
isPrime n
	| n <= 1 = False
	| otherwise = (n ==) $ head $ primeFactors n

-- | Checks wether two integers are coprimes (or mutual primes), i.e. gcd(a,b)
-- == 1
areCoprimes :: Integral a => a -> a -> Bool
areCoprimes x y = (gcd x y) == 1

-- |Returns the list of the prime factors and their coefficients in the
-- factorisation of an integer
factorize :: Integral a => a -> [ (a, Int) ]
factorize n = map (\ xs -> (head xs, length xs)) $ group $ primeFactors n

{-[ Sequences ]-}

-- |Sequence of primes (A000040)
-- Note this is a relatively efficient implementation, as it builds the list of
-- primes based on itself, and only computes the divisions with respect to
-- existing primes. There is nonetheless a tail-append, which is not cool.
primes :: Integral a => [a]
primes = primesWithList 0 []

-- |Sequence of composite (i.e. non-prime) numbers (A002808)
composites :: Integral a => [a]
composites = [ x | x <- [2..], not $ isPrime x ]

-- |Computes the next element in a Collatz sequence
collatz :: Integral a => a -> a
collatz n
	| even n    = n `div` 2
	| otherwise = 3 * n + 1

-- |Compputes the i-th term of the Fibonacci sequence
fib :: Integral a => Int -> a
fib n = fibs!!n

-- |The full Fibonacci sequence (A000045)
fibs :: Integral a => [a]
fibs = fibHelper 0 1
	where
		fibHelper a b = a : (fibHelper b $ a+b)

-- |The successive digits of the Champerowne constant
champerowne :: Integral a => [a]
champerowne = 0 : concatMap intToList [1..]

-- |The sequence of triangular numbers (A000217)
triangulars :: Integral a => [a]
triangulars = tail $ scanl (+) 0 [1..]

-- |The sequence of square numbers (A000290)
squares :: Integral a => [a]
squares = [ x*x | x <- [0..] ]

-- |The sequence of pentagonal numbers (A000326)
pentagonals :: Integral a => [a]
pentagonals = [ (3 * n * n - n) `div` 2 | n <- [1..] ]

-- |The sequence of hexagonal numbers (A000384)
hexagonals :: Integral a => [a]
hexagonals = [ (2 * n * n - n) | n <- [1..] ]

-- |The sequence of heptagonal numbers (A000566)
heptagonals :: Integral a => [a]
heptagonals = [ (5 * n * n - 3 * n) `div` 2 | n <- [1..] ]

-- |The sequence of octagonal numbers (A000567)
octagonals :: Integral a => [a]
octagonals = [ (3 * n * n - 2 * n) | n <- [1..] ]

-- |Approximative list of Lychrel numbers (capped at 25 iterations, may not work
-- properly above 1000).
lychrels :: (Read a, Integral a, Show a) => [a]
lychrels = [ x | x <- [ 1 .. ], isLychrel x ]

{-[ Dates ]-}

-- |Returns true if a given year contains a leap day (no bound checks).
isLeapYear :: Integral a => a -> Bool
isLeapYear y
	| y `mod` 400 == 0 = True
	| y `mod` 100 == 0 = False
	| y `mod` 4   == 0 = True
	| otherwise        = False

-- |Returns 1 if there is a leap day in a given year, 0 otherwise.
leapDay :: Integral a => a -> a
leapDay y
	| isLeapYear y = 1
	| otherwise    = 0

-- |Length of the months for a given year (includes leap day computation).
monthLengths :: Integral a => a -> [a]
monthLengths y = [31,28 + leapDay y,31,30,31,30,31,31,30,31,30,31]

{-[ Conversions ]-}

-- |Converts an integer in a list of its digits in a given base.
intToListWithBase :: Integral a => a -> a -> [a]
intToListWithBase _ 0 = []
intToListWithBase b n =  intToListWithBase b (n `div` b) ++ [n `mod` b]

-- |Converts an integer in a list of its digits in base 10.
intToList :: Integral a => a -> [a]
intToList n = intToListWithBase 10 n

-- |Converts a list of digits in an integer (fails if some are greater than
-- nine).
listToInt :: Integral a => [a] -> a
listToInt ds = foldl (+) 0 $ zipWith (\ x y -> x*10^y) (reverse ds) ps
	where ps = [0..] :: [Integer]

letterToInt :: Integral a => Char -> a
letterToInt c
	| isAsciiUpper c = fromIntegral $ (ord c) - (ord 'A') + 1
	| isAsciiLower c = fromIntegral $ (ord c) - (ord 'a') + 1
	| otherwise = error "letterToInt: not an ASCII letter"

{-[ Signal processing ]-}

-- |Returns the cross-correlation between the output of two functions, at a
-- given point.
crossCorrelation :: Integral a => [a] -> [a] -> a -> a
crossCorrelation fs gs n = sum $ zipWith (*) fs (drop n' gs)
	where n' = fromIntegral n

-- |Auto-correlation is a cross-correlation between a function and itself.
autoCorrelation :: Integral a => [a] -> a -> a
autoCorrelation fs n = crossCorrelation fs fs n

-- |Computes the whole cross-correlation with all possible shifts between the
-- output of two functions.
doCrossCorrelation :: Integral a => [a] -> [a] -> [a]
doCrossCorrelation fs gs = map (crossCorrelation fs gs) [0..m]
	where m = fromIntegral $ max (length fs) (length gs)

-- |Computes the full auto-correlation of a function, with all shifts.
doAutoCorrelation :: Integral a => [a] -> [a]
doAutoCorrelation fs = doCrossCorrelation fs fs

{-[ Lists ]-}

-- |Rotates a list by one (takes the last element and puts it first).
rotateR :: [a] -> [a]
rotateR [] = []
rotateR xs = last xs : init xs

-- |Rotates a list by one (takes the first element and puts it last).
rotateL :: [a] -> [a]
rotateL [] = []
rotateL xs = tail xs ++ [head xs]

-- |Gives all possible rotations of a list.
rotations :: Eq a => [a] -> [[a]]
rotations xs = rotateHelper xs xs

-- |Rotation helper function (builds the rotation list).
rotateHelper :: Eq a => [a] -> [a] -> [[a]]
rotateHelper xs rs
	| xs' == rs  = [xs]
	| otherwise = xs:rotateHelper xs' rs
	where
		xs' = rotateR xs

{-[ Number properties ]-}

-- |Returns true if a number contains all digits between two given $n$ and $m$,
-- once.
isNMPandigital :: Integral a => a -> a -> a -> Bool
isNMPandigital m n x = (sort $ intToList $ x) == ([m..n])

-- |Returns true if a number contains all digits from one to a given $n$, once.
isNPandigital :: Integral a => a -> a -> Bool
isNPandigital= isNMPandigital 1

-- |Returns true if a number contains all digits once.
isPanDigital :: Integral a => a -> Bool
isPanDigital = isNPandigital 9

-- |Returns true if a number is a palindrome in a given base.
isPalindromeWithBase :: Integral a => a -> a -> Bool
isPalindromeWithBase b n = l == reverse l
	where l = intToListWithBase b n

-- |Returns true if a number is a palindrome in base 10
isPalindrome :: Integral a => a -> Bool
isPalindrome = isPalindromeWithBase 10

-- |Computes the length of an integer (logarithm do not work because of rounding
-- errors).
intLength :: Integral a => a -> a
intLength n = h n 10
	where
		h :: Integral a => a -> a -> a
		h n' b
			| n' == 0 = 0
			| otherwise       = 1 + h (n' `div` b) b

-- |Returns true if the number is a triangular number
isTriangular :: Integral a => a -> Bool
isTriangular x
	| isNothing x'               = False
	| (fromJust x') `mod` 2 /= 1 = False
	| otherwise                  = True
	where
		x' = intSqrt $ 8 * x + 1

-- |Returns true if the number is a square number
isSquare :: Integral a => a -> Bool
isSquare x
	| isNothing x'               = False
	| otherwise                  = True
	where
		x' = intSqrt $ x

-- |Returns true if the number is a pentagonal number
isPentagonal :: Integral a => a -> Bool
isPentagonal x
	| isNothing x'               = False
	| (fromJust x') `mod` 6 /= 5 = False
	| otherwise                  = True
	where
		x' = intSqrt $ 24 * x + 1

-- |Returns true if the number is an hexagonal number
isHexagonal :: Integral a => a -> Bool
isHexagonal x
	| isNothing x'               = False
	| (fromJust x') `mod` 4 /= 3 = False
	| otherwise                  = True
	where
		x' = intSqrt $ 8 * x + 1

-- |Returns true if the number is an heptagonal number
isHeptagonal :: Integral a => a -> Bool
isHeptagonal x
	| isNothing x'                = False
	| (fromJust x') `mod` 10 /= 7 = False
	| otherwise                   = True
	where
		x' = intSqrt $ 40 * x + 9

-- |Returns true if the number is an octagonal number
isOctagonal :: Integral a => a -> Bool
isOctagonal x
	| isNothing x'                = False
	| (fromJust x') `mod` 3 /= 2 = False
	| otherwise                   = True
	where
		x' = intSqrt $ 3 * x + 1

-- |Returns True if the number is a Lychrel number (does not produce a
-- palindrome when recursively summed with its reverse). Capped to 25
-- iterations, as all numbers below 10000 verify this property.
isLychrel :: (Integral a, Read a, Show a) => a -> Bool
isLychrel n = not $ any isPalindrome $ take 25 $ iterate f $ f n
	where f x = x + reverseInt x

-- |Reverse the digits of a number
reverseInt :: (Integral a, Read a, Show a) => a -> a
reverseInt = read . reverse . show

-- |Returns the digital root (recursive sum of digits)
digitalRoot :: Integral a => a -> a
digitalRoot n
	| n < 10    = n
	| otherwise = digitalRoot $ sum $ intToList n

{-[ Arithmetic functions ]-}

-- |Factorial of a number.
fact :: Integral a => a -> a
fact n = product [1..n]

-- |Combinatorial operator (select r from n)
comb :: Integral a => a -> a -> a
comb r n
	| r < 0     = error "comb: r must be positive."
	| r > n     = error "comb: r must be less than n."
	| otherwise = (fact n) `div` ((fact r) * (fact (n-r)))

-- |Returns the nth-root of an integer if an integer, Nothing otherwise
intNthRoot :: Integral a => a -> a -> Maybe a
intNthRoot r n
	| n == n'^r = Just n'
	| otherwise    = Nothing
	where
		n' = round $ exp $ ( log ( fromIntegral n :: Double ) / ( fromIntegral r :: Double ) )

-- |Returns the square root of an integer if it is a perfect square, nothing
-- otherwise (we do not use the nth-root algorithm above since sqrt is probably
-- more optimized than exponential and logarithm).
intSqrt :: Integral a => a -> Maybe a
intSqrt n
	| n == n' * n' = Just n'
	| otherwise    = Nothing
	where
		n' = round $ sqrt $ ( fromIntegral n :: Double )

intLog :: Integral a => a -> a -> a
intLog n = h 0
	where
		h c x
			| x == 0    = c - 1
			| otherwise = h (c + 1) (x `div` n)

-- |Returns the binary logarithm of a given integer
lb :: Integral a => a -> a
lb = intLog 2
--
-- |Returns the binary logarithm of a given integer
ld :: Integral a => a -> a
ld = intLog 10

-- |Concatenates two integers
intConcat :: Integral a => a -> a -> a
intConcat x y = x*s+y
	where
		s = head $ dropWhile (<=y) $ iterate (*10) 1

-- |Returns the Euler Totient of a given integer (the number of coprimes less
-- than the integer).
totient :: Integral a => a -> a
totient n = product $ factorTotients $ primeFactors n

-- |Small helper function to help computing the totient of an integer
factorTotients :: Integral a => [a] -> [a]
factorTotients ps = map (\ xs -> t (head xs, length xs)) $ group ps
	where
		t (p, k) = p^(k-1)*(p-1)
