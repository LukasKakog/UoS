{-# LANGUAGE DeriveGeneric #-}
--TEMPLATE FILE FOR COURSEWORK 1 for COMP2209
--Julian Rathke, Oct 2022

--EXERCISE A4 ONLY

--CONTAINS FUNCTION REQIURED FOR COMPILATION AGAINST THE TEST SUITE
--MODIFY THE FUNCTION DEFINITIONS WITH YOUR OWN SOLUTIONS
--IMPORTANT : DO NOT MODIFY ANY FUNCTION TYPES


module Exercises (connected) where

import GHC.Generics (Generic,Generic1)
import Control.DeepSeq
import Data.List 
import Data.Function

-- Exercise A4

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
coords :: ((Integer,Integer),(Integer,Integer)) -> [(Integer,Integer)]
coords ((a,b),(c,d)) | (a == c) && (b == d) = [(a,b)]
                     | (a == c) && (b < d) = [(a,y) | y <- [b..d]]
                     | (a == c) && (b > d) = [(a,y) | y <- [d..b]]
                     | (b == d) && (a < c) = [(x,b) | x <- [a..c]]
                     | (b == d) && (a > c) = [(x,b) | x <- [c..a]]

listOfCoords :: [((Integer,Integer),(Integer,Integer))] -> [[(Integer,Integer)]]
listOfCoords = map coords  

allCoords :: [((Integer,Integer),(Integer,Integer))] -> [(Integer,Integer)]
allCoords xs = sortingPairs (sortingPairs (concat $ listOfCoords xs) 2) 1

--Checking 
comparing :: Eq a => [a] -> [a] -> Bool  
comparing (x:xs) ys | (xs == []) && (elem x ys == False) = False
                    | (xs == []) && (elem x ys == True) = True
                    | elem x ys == False = comparing xs ys
                    | elem x ys == True = True

checking :: [[(Integer, Integer)]] -> [(Integer,Integer)] -> [(Integer,Integer)]
checking (x:xs) ys | (x:xs) == [] = ys
                   | (ys == []) && (length (x:xs) == 1) = ys ++ x
                   | (ys == []) && (length (x:xs) > 1) = checking xs (ys ++ x)
                   | (xs == []) && (comparing x ys == False) = ys
                   | (xs == []) && (comparing x ys == True) = ys ++ x
                   | comparing x ys == False = checking xs ys
                   | comparing x ys == True = checking xs (ys ++ x)

connected :: [((Integer, Integer), (Integer, Integer))] -> Bool
connected ms | ms == [] = True
             | allCoords ms == (sortingPairs (sortingPairs (checking (listOfCoords ms) []) 2) 1) = True
             | otherwise = False