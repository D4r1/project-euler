module Euler.E17 where
euler17 :: Int -> Int
euler17 n = sum $ map (length . numToString) [1..n]

zero :: String
zero = ""
one :: String
one = "one"
two :: String
two = "two"
three :: String
three = "three"
four :: String
four = "four"
five :: String
five = "five"
six :: String
six = "six"
seven :: String
seven = "seven"
eight :: String
eight = "eight"
nine :: String
nine = "nine"
ten :: String
ten = "ten"
eleven :: String
eleven = "eleven"
twelve :: String
twelve = "twelve"
thirteen :: String
thirteen = "thirteen"
fourteen :: String
fourteen = "fourteen"
fifteen :: String
fifteen = "fifteen"
sixteen :: String
sixteen = "sixteen"
seventeen :: String
seventeen = "seventeen"
eighteen :: String
eighteen = "eighteen"
nineteen :: String
nineteen = "nineteen"
twenty :: String
twenty = "twenty"
thirty :: String
thirty = "thirty"
forty :: String
forty = "forty"
fifty :: String
fifty = "fifty"
sixty :: String
sixty = "sixty"
seventy :: String
seventy = "seventy"
eighty :: String
eighty = "eighty"
ninety :: String
ninety = "ninety"
hundred :: String
hundred = "hundred"
thousand :: String
thousand = "thousand"
and' :: String
and' = "and"

units :: [String]
units = [zero,one,two,three,four,five,six,seven,eight,nine,ten,eleven,twelve,thirteen,fourteen,fifteen,sixteen,seventeen,eighteen,nineteen]

tens :: [String]
tens = [zero,ten,twenty,thirty,forty,fifty,sixty,seventy,eighty,ninety]

numToString :: Int -> String
numToString n
	| n <= 19 = units !! n
	| n <= 99 = (tens!!(n`div` 10)) ++ (numToString (n `mod` 10))
	| n <= 999 = (numToString (n `div` 100)) ++ hundred ++ and'' ++ (numToString n')
	| n == 1000 = one ++ thousand
	| otherwise = "over nine thousand!"
	where
		n' = n `mod` 100
		and'' = if n' == 0 then zero else and'

main :: IO ()
main = print $ euler17 1000
