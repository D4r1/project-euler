module Euler.E59 where

import Data.Word (Word8)
import Data.Char (ord, chr, isControl)
import Data.Bits (xor)
import Data.List (isInfixOf, intersect)

type CipherText = [Word8]
type ClearText = [Word8]
type Key = [Word8]

commonWords :: [ClearText]
commonWords = map (\s -> map (fromIntegral . ord) s) $
	[ "the"
	, "be"
	, "to"
	, "of"
	, "and"
	, "a"
	, "in"
	, "that"
	, "have"
	, "I"
	]

controlChars :: ClearText
controlChars = filter (isControl . chr . fromIntegral) [0..255]

forbiddenChars :: ClearText
forbiddenChars = map (fromIntegral . ord) $ "=#~[]{}&^_/\\|"

euler59 :: String -> Int
euler59 s = sum $ map fromIntegral $ decrypt c $ head $ candidateKeys c
	where
		c = getText s

candidateKeys :: CipherText -> [Key]
candidateKeys c =
	[ [k1,k2,k3]
	| k1 <- [97..122]
	, k2 <- [97..122]
	, k3 <- [97..122]
	, null $ intersect controlChars $ decrypt c [k1,k2,k3]
	, null $ intersect forbiddenChars $ decrypt c [k1,k2,k3]
	, (findWords $ decrypt c [k1,k2,k3]) > 5
	]

findWords :: ClearText -> Int
findWords t = length $ filter (findWord t) commonWords

findWord :: ClearText -> ClearText -> Bool
findWord t w = w `isInfixOf` t

getText :: String -> CipherText
getText s = read s

decrypt :: CipherText -> Key -> ClearText
decrypt c k = zipWith xor c $ cycle k

render :: ClearText -> String
render t = map (chr . fromIntegral) t

main :: IO ()
main = interact $ show . euler59
