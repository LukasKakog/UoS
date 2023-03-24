{-# LANGUAGE DeriveGeneric #-}
--SKELETON FILE FOR HANDIN 1 OF COURSEWORK 1 for COMP2209, 2021
--CONTAINS ALL FUNCTIONS REQIURED FOR COMPILATION AGAINST THE TEST SUITE
--MODIFY THE FUNCTION DEFINITIONS WITH YOUR OWN SOLUTIONS
--IMPORTANT : DO NOT MODIFY ANY FUNCTION TYPES
--Julian Rathke, Oct 2022

module Exercises (vigenere,frequency,renderMaze) where

-- The following two imports are needed for testing, do not delete
import GHC.Generics (Generic,Generic1)
import Control.DeepSeq
import Data.Char
import Data.List
import Data.Function

-- Exercise A1

-- Shifting a given character to decode/encode it
convert :: (Int -> Int -> Int) -> Char -> Char -> Char
convert op phrase key = cFromAscii ((cToAscii phrase) `op` (cToAscii key))
                        where cToAscii phrase = ord phrase - 65
                              cFromAscii ascii = chr (65 + (ascii `mod` 26))

--check if String is all Uppercase and only from A to Z
check :: String -> String
check [] = []
check (a:as) = (filter isLetter [toUpper a]) ++ check as

--return ecryption and decryption functions based on a key a
vigenere :: String -> (String -> String, String -> String)
vigenere a | a == [] = error "Empty String"
           | otherwise = (encr, decr)
             where encr str = zipWith (convert (+)) (check str) (cycle $ check a)
                   decr str = zipWith (convert (-)) (check str) (cycle $ check a)

-- Exercise A2

--Sorting the number of occurences of each character in descending order
sortingOccurences :: [(Char,Int)] -> [(Char,Int)]
sortingOccurences xs = reverse $ sortBy (compare `on` snd) xs

--Dividing a given ciphertext into its frequency lists
frequencyCount :: String -> [(Char,Int)]
frequencyCount xs = reverse [ (char,n) | char <- ['A'..'Z'], let n = (length . filter (==char)) xs, n > 0 ]

findList :: Int -> Int -> Int -> String -> String  
findList n i x ct | (i*n + x) > ((length ct) - 1) = []
                  | otherwise = [ct !! (i * n + x)] ++ findList n (i + 1) x ct

anyList :: Int -> Int -> String -> [String]  
anyList n x ct | x < n = [findList n 0 x ct] ++ anyList n (x + 1) ct
               | otherwise = []

--Returns a list of n frequency lists for the characters in the ciphertext
frequency :: Int -> String -> [[(Char, Int)]]
frequency n ct | n <= 0 = error "Size has to be greater than 0"
               | otherwise = map (sortingOccurences . frequencyCount) (anyList n 0 ct)

-- Exercise A3

--Sorting pairs by either ascending x or ascending y
sortingPairs :: [(Integer,Integer)] -> Int -> [(Integer,Integer)]
sortingPairs xs b | b == 1 = sortBy (compare `on` fst) xs
                  | b == 2 = sortBy (compare `on` snd) xs

--Finding largest x and y in the given coordinates
toList :: (Eq a) => [(a,a)] -> [a]
toList ((x,y):xs) | xs == [] = x : y : []
                  | otherwise = x : y : toList xs

findLargest :: [(Integer,Integer)] -> (Integer,Integer)
findLargest xs = head [(x,y) | x <- [fst $ last $ sortingPairs xs 1], y <- [snd $ last $ sortingPairs xs 2]]

--Finding all coords in the grid that should be a path
allCoords :: [((Integer,Integer),(Integer,Integer))] -> [(Integer,Integer)]
allCoords xs = sortingPairs (sortingPairs (concat $ map coords xs) 1) 2
               where coords :: ((Integer,Integer),(Integer,Integer)) -> [(Integer,Integer)]
                     coords ((a,b),(c,d)) | (a == c) && (b == d) = [(a,b)]
                                          | (a == c) && (b < d) = [(a,y) | y <- [b..d]]
                                          | (a == c) && (b > d) = [(a,y) | y <- [d..b]]
                                          | (b == d) && (a < c) = [(x,b) | x <- [a..c]]
                                          | (b == d) && (a > c) = [(x,b) | x <- [c..a]]

--Creating the grid for the maze
doHash :: Bool -> Char
doHash boolean | boolean == False = ' '
               | otherwise = '#'

renderMaze :: [((Integer, Integer), (Integer, Integer))] -> [String]
renderMaze ms | ms == [] = []
              | otherwise = [[doHash (elem (x,y) (allCoords ms)) | x <- [0.. (fst $ findLargest $ toList ms)]] | y <- [0.. (snd $ findLargest $ toList ms)]]