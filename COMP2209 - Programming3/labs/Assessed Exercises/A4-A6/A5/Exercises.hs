
module Exercises (pathFinder,Dir(..),Strategy) where

import Data.List
import Data.Function

type Strategy = Dir -> [Dir] -> Maybe Dir
data Dir = U | D | L | R deriving (Eq,Ord,Show,Read) 

--A3--

--Sorting pairs by either ascending x or ascending y
sortingPairs :: [(Integer,Integer)] -> Int -> [(Integer,Integer)]
sortingPairs xs b | b == 1 = sortBy (compare `on` fst) xs
                  | b == 2 = sortBy (compare `on` snd) xs

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

--A5--

--Finding all possible directions from current node
checkForSamePath :: [[(Integer,Integer)]] -> (Integer,Integer) -> (Integer,Integer) -> Bool  
checkForSamePath (x:xs) a b | xs == [] = (elem a x) && (elem b x)
                            | (elem a x) && (elem b x) = True 
                            | otherwise = checkForSamePath xs a b 

findUp :: [((Integer,Integer),(Integer,Integer))] -> Maybe (Integer,Integer) -> [[(Integer,Integer)]] -> [Dir]
findUp ms (Just (x,y)) xs | (elem (x,y-1) (allCoords ms)) && (checkForSamePath xs (x,y) (x,y-1)) = [U]
                          | otherwise = []

findDown :: [((Integer,Integer),(Integer,Integer))] -> Maybe (Integer,Integer) -> [[(Integer,Integer)]] -> [Dir]
findDown ms (Just (x,y)) xs | (elem (x,y+1) (allCoords ms)) && (checkForSamePath xs (x,y) (x,y+1)) = [D]
                            | otherwise = []

findLeft :: [((Integer,Integer),(Integer,Integer))] -> Maybe (Integer,Integer) -> [[(Integer,Integer)]] -> [Dir]
findLeft ms (Just (x,y)) xs | (elem (x-1,y) (allCoords ms)) && (checkForSamePath xs (x,y) (x-1,y)) = [L]
                            | otherwise = []

findRight :: [((Integer,Integer),(Integer,Integer))] -> Maybe (Integer,Integer) -> [[(Integer,Integer)]] -> [Dir]
findRight ms (Just (x,y)) xs | (elem (x+1,y) (allCoords ms)) && (checkForSamePath xs (x,y) (x+1,y)) = [R]
                             | otherwise = []

findAllDir :: [((Integer,Integer),(Integer,Integer))] -> Maybe (Integer,Integer) -> [Dir] 
findAllDir ms (Just (x,y)) | elem (x,y) (allCoords ms) = findUp ms (Just (x,y)) (listOfCoords ms) ++ findDown ms (Just (x,y)) (listOfCoords ms) ++ findLeft ms (Just (x,y)) (listOfCoords ms) ++ findRight ms (Just (x,y)) (listOfCoords ms)
                           | otherwise = []

--function to move one coordinate down a path
move :: [((Integer,Integer),(Integer,Integer))] -> Strategy -> (Maybe (Integer,Integer), Maybe Dir) -> Maybe (Integer,Integer)
move ms strat (Nothing, _) = Nothing
move ms strat (_, Nothing) = Nothing
move ms strat (Just (x,y), Just direc) | strat direc (findAllDir ms (Just (x,y))) == Just U = Just (x,y-1)
                                       | strat direc (findAllDir ms (Just (x,y))) == Just D = Just (x,y+1)
                                       | strat direc (findAllDir ms (Just (x,y))) == Just L = Just (x-1,y)
                                       | strat direc (findAllDir ms (Just (x,y))) == Just R = Just (x+1,y)
                                       | otherwise = Nothing

run :: [((Integer,Integer),(Integer,Integer))] -> Strategy -> (Maybe (Integer,Integer), Maybe Dir) -> (Integer,Integer) -> Bool  
run ms strat (Nothing, _) final = False
run ms strat (_, Nothing) final = False
run ms strat (Just current, Just direc) final | current == final = True
                                              | otherwise = run ms strat (move ms strat (Just current, Just direc), nextDir) final
                                                 where nextDir = strat direc (findAllDir ms (Just current))

pathFinder :: [((Integer,Integer),(Integer,Integer))] -> Strategy -> ((Integer,Integer),Dir) -> (Integer,Integer) -> Bool
pathFinder ms strat (initial, direc) final | ms == [] = False
                                           | elem initial (allCoords ms) == False = False
                                           | initial == final = run ms strat (move ms strat (Just initial, Just direc), nextDir) final
                                           | otherwise = run ms strat (Just initial, Just direc) final 
                                              where nextDir = strat direc (findAllDir ms (Just initial))