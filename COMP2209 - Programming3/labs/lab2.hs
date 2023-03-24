
import Data.Char

--ex4

halve :: [a] -> ([a],[a])
halve [] = error "Empty List"
halve xs = (ls, rs)
	where ls = take (length(xs) `div` 2) xs
	      rs = drop (length(xs) `div` 2) xs

--ex5

encrypt :: Int -> String -> (String , String -> String)
encrypt a str = (enc a str , decr)
        where enc a (x:secret) = [chr ((ord x) + a)] ++ enc a secret
              enc a [] = []
              decr (x:str) = [chr ((ord x) - a)] ++ decr str
              decr [] = []

--ex6

gNumber :: String -> Int
gNumber ('A':'*':gr) = 56 + gNumber gr
gNumber ('A':gr) = 48 + gNumber gr
gNumber ('B':gr) = 40 + gNumber gr
gNumber ('C':gr) = 32 + gNumber gr
gNumber ('D':gr) = 24 + gNumber gr
gNumber ('E':gr) = 16 + gNumber gr
gNumber [] = 0
gNumber (_:gr) = error "Not valid grade"

meetsOffer :: String -> Int -> Bool
meetsOffer grades offer | (gNumber grades) < offer = False
                        | otherwise = True 

--ex7

luhnDouble :: Int -> Int
luhnDouble x | x > 4 = x * 2 - 9
             | otherwise = x * 2

luhnAdd :: Int -> Int -> Int -> Int -> Int
luhnAdd a b c d = luhnDouble(a) + b + luhnDouble(c) + d

luhn :: Int -> Int -> Int -> Int -> Bool
luhn a b c d | (luhnAdd a b c d) `mod` 10 > 0 = False
             | otherwise = True