{-# LANGUAGE DeriveGeneric #-}
--TEMPLATE FILE FOR COURSEWORK 1 for COMP2209
--Julian Rathke, Oct 2022

--EXERCISE A3 ONLY

--CONTAINS FUNCTION REQIURED FOR COMPILATION AGAINST THE TEST SUITE
--MODIFY THE FUNCTION DEFINITIONS WITH YOUR OWN SOLUTIONS
--IMPORTANT : DO NOT MODIFY ANY FUNCTION TYPES


module Exercises (renderMaze) where

-- The following two imports are needed for testing, do not delete
import GHC.Generics (Generic,Generic1)
import Control.DeepSeq
import Data.List
import Data.Function

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

doHash :: Bool -> Char
doHash boolean | boolean == False = ' '
               | otherwise = '#'

renderMaze :: [((Integer, Integer), (Integer, Integer))] -> [String]
renderMaze ms | ms == [] = []
              | otherwise = [ [doHash (elem (x,y) (allCoords ms)) | x <- [0.. (fst $ findLargest $ toList ms)]] | y <- [0.. (snd $ findLargest $ toList ms)]]