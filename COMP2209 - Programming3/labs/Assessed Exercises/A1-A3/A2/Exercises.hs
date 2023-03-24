{-# LANGUAGE DeriveGeneric #-}
--TEMPLATE FILE FOR COURSEWORK 1 for COMP2209
--Julian Rathke, Oct 2022

--EXERCISE A2 ONLY

--CONTAINS FUNCTION REQIURED FOR COMPILATION AGAINST THE TEST SUITE
--MODIFY THE FUNCTION DEFINITIONS WITH YOUR OWN SOLUTIONS
--IMPORTANT : DO NOT MODIFY ANY FUNCTION TYPES


module Exercises (frequency) where

-- The following two imports are needed for testing, do not delete
import GHC.Generics (Generic,Generic1)
import Control.DeepSeq
import Data.List
import Data.Function


-- Exercise A2
sortingOccurences :: [(Char,Int)] -> [(Char,Int)]
sortingOccurences xs = reverse $ sortBy (compare `on` snd) xs

frequencyCount :: String -> [(Char,Int)]
frequencyCount xs = reverse [ (char,n) | char <- ['A'..'Z'], let n = (length . filter (==char)) xs, n > 0 ]

findList :: Int -> Int -> Int -> String -> String  
findList n i x ct | (i*n + x) > ((length ct) - 1) = []
                  | otherwise = [ct !! (i * n + x)] ++ findList n (i + 1) x ct

anyList :: Int -> Int -> String -> [String]  
anyList n x ct | x < n = [findList n 0 x ct] ++ anyList n (x + 1) ct
               | otherwise = []

frequency :: Int -> String -> [[(Char, Int)]]
frequency n ct | n <= 0 = error "Size has to be greater than 0"
               | otherwise = map (sortingOccurences . frequencyCount) (anyList n 0 ct)