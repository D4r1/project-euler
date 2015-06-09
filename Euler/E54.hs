module Euler.E54 where

import Data.List (sort, nub)
import Data.Maybe
import Euler.Lib (rotations)

data Rank = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King | Ace
	deriving (Eq, Ord, Show, Enum)

data Suit = Hearts | Spades | Diamonds | Clubs
	deriving (Eq, Ord, Show)

data Card = Card
	{ rank :: Rank
	, suit :: Suit
	}
	deriving (Eq, Ord)

data HandVal =
	HighCard [Rank]
	| OnePair Rank HandVal
	| TwoPairs Rank Rank HandVal
	| ThreeOfAKind Rank HandVal
	| Straight Rank
	| Flush Suit HandVal
	| FullHouse Rank Rank
	| FourOfAKind Rank HandVal
	| StraightFlush Rank
	| RoyalFlush Suit
	deriving (Eq, Ord, Show)

type Hand = [Card]

instance Read Rank where
	readsPrec x (' ':s) = readsPrec x s
	readsPrec _ ('2':s) = [(Two, s)]
	readsPrec _ ('3':s) = [(Three, s)]
	readsPrec _ ('4':s) = [(Four, s)]
	readsPrec _ ('5':s) = [(Five, s)]
	readsPrec _ ('6':s) = [(Six, s)]
	readsPrec _ ('7':s) = [(Seven, s)]
	readsPrec _ ('8':s) = [(Eight, s)]
	readsPrec _ ('9':s) = [(Nine, s)]
	readsPrec _ ('T':s) = [(Ten, s)]
	readsPrec _ ('J':s) = [(Jack, s)]
	readsPrec _ ('Q':s) = [(Queen, s)]
	readsPrec _ ('K':s) = [(King, s)]
	readsPrec _ ('A':s) = [(Ace, s)]
	readsPrec _ s       = error $ "read Rank: could not parse string '" ++ s ++ "'."

instance Read Suit where
	readsPrec x (' ':s) = readsPrec x s
	readsPrec _ ('H':s) = [(Hearts, s)]
	readsPrec _ ('S':s) = [(Spades, s)]
	readsPrec _ ('D':s) = [(Diamonds, s)]
	readsPrec _ ('C':s) = [(Clubs, s)]
	readsPrec _ s       = error $ "read Suit: could not parse string '" ++ s ++ "'."

instance Show Card where
	show (Card r s) = show r ++ " of " ++ show s

instance Read Card where
	readsPrec x (' ':s) = readsPrec x s
	readsPrec _ (r:s:x) = [(Card (read [r]) (read [s]), x)]
	readsPrec _ s       = error $ "read Card: could not parse string '" ++ s ++ "'."

instance Enum Card where
	toEnum i = Card (toEnum i :: Rank) Spades
	fromEnum (Card r _) = fromEnum r

euler54 :: [String] -> Int
euler54 ss = length $ filter isWinnerPlayer1 ss

debug :: String -> String
debug s = s ++ " -> " ++ (show v1) ++ " | " ++ (show v2) ++ " | " ++ (show $ v1 > v2)
	where
		(h1, h2) = mkHands 5 s
		v1 = getHandVal h1
		v2 = getHandVal h2

isWinnerPlayer1 :: String -> Bool
isWinnerPlayer1 s = v1 > v2
	where
		(h1, h2) = mkHands 5 s
		v1 = getHandVal h1
		v2 = getHandVal h2

mkHands :: Int -> String -> (Hand, Hand)
mkHands n s = readHands n $ words s

readHands :: Int -> [String] -> (Hand, Hand)
readHands n ss = (h1, h2)
	where
		cs = map (\s -> read s :: Card) ss
		h1 = sort $ take n cs
		h2 = sort $ take n $ drop n cs

getHandVal :: Hand -> HandVal
getHandVal h
	| isJust $ rf = RoyalFlush (fromJust rf)
	| isJust $ sf = StraightFlush (fromJust sf)
	| isJust $ fk = FourOfAKind (fst $ fromJust fk) (HighCard $ s' $snd $ fromJust fk)
	| isJust $ fh = FullHouse (fst $ fromJust fh) (snd $ fromJust fh)
	| isJust $ f  = Flush (fromJust f) (HighCard $ s' rs)
	| isJust $ s  = Straight (fromJust s)
	| isJust $ tk = ThreeOfAKind (fst $ fromJust tk) (HighCard $ s' $ snd $ fromJust tk)
	| isJust $ tp = TwoPairs (head $ fst $ fromJust tp) (last $ fst $ fromJust tp) (HighCard $ s' $snd $ fromJust tp)
	| isJust $ p  = OnePair (fst $ fromJust p) (HighCard $ s' $snd $ fromJust p)
	| otherwise = HighCard (s' rs)
	where
		rs = map rank h
		ss = map suit h
		rf = isRoyalFlush h
		sf = isStraightFlush h
		fk = isFourOfAKind rs
		fh = isFullHouse rs
		f  = isFlush ss
		s  = isStraight rs
		tk = isThreeOfAKind rs
		tp = isTwoPairs rs
		p  = isPair rs
		s' = reverse . sort

isU :: Eq a => [a] -> Bool
isU xs = (length $ nub xs) == 1

-- This one works with any size of hand!
isNSame :: Int -> [Rank] -> Maybe (Rank,[Rank])
isNSame n r
	| not $ null ps = Just (head $ head ps, sort $ drop n $ head ps)
	| otherwise = Nothing
	where
		ps = filter (isU . (take n)) $ rotations r

isPair :: [Rank] -> Maybe (Rank,[Rank])
isPair r = isNSame 2 r

isTwoPairs :: [Rank] -> Maybe ([Rank], [Rank])
isTwoPairs r
	| and [ isJust p1, isJust p2 ] = Just ([fst $ fromJust p1, fst $ fromJust p2], snd $ fromJust p2)
	| otherwise = Nothing
	where
		p1 = isPair r
		p2 = isPair (snd $ fromJust p1)

isThreeOfAKind :: [Rank] -> Maybe (Rank,[Rank])
isThreeOfAKind r = isNSame 3 r

isStraight :: [Rank] -> Maybe Rank
isStraight r
	| r == [head r .. last r] = Just $ head r
	| otherwise = Nothing

isFlush :: [Suit] -> Maybe Suit
isFlush s
	| (isU s) = Just (head s)
	| otherwise = Nothing

isFullHouse :: [Rank] -> Maybe (Rank,Rank)
isFullHouse r
	| and [ isJust tk, isJust $ isPair r' ] = Just (r1, r'!!0)
	| otherwise = Nothing
	where
		tk = isThreeOfAKind r
		(r1,r') = fromJust tk

isFourOfAKind :: [Rank] -> Maybe (Rank, [Rank])
isFourOfAKind r = isNSame 4 r

isStraightFlush :: Hand -> Maybe Rank
isStraightFlush h
	| and [isJust $ isStraight r, isJust $ isFlush s] = Just $ head r
	| otherwise = Nothing
	where
		r = map rank h
		s = map suit h

isRoyal :: [Rank] -> Bool
isRoyal r = r == [Ten, Jack, Queen, King, Ace]

isRoyalFlush :: Hand -> Maybe Suit
isRoyalFlush h
	| and [isJust $ isFlush s, isRoyal r] = Just $ suit $ head h
	| otherwise = Nothing
	where
		r = map rank h
		s = map suit h

main :: IO ()
main = interact $ show . euler54 . lines
