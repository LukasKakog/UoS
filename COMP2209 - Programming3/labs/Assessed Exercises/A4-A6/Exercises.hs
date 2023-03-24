{-# LANGUAGE DeriveGeneric #-}
--SKELETON FILE FOR HANDIN 2 OF COURSEWORK 1 for COMP2209, 2020
--CONTAINS ALL FUNCTIONS REQIURED FOR COMPILATION AGAINST THE TEST SUITE
--MODIFY THE FUNCTION DEFINITIONS WITH YOUR OWN SOLUTIONS
--IMPORTANT : DO NOT MODIFY ANY FUNCTION TYPES
--Julian Rathke, Oct 2020

module Exercises (connected,pathFinder,Dir(..),Strategy,insertFromCurrentNode,VTree(..),Direction(..),Trail(..),Zipper(..)) where

-- The following two  imports are needed for testing, do not delete
import GHC.Generics (Generic,Generic1)
import Control.DeepSeq
-- Put ALL of your own import statements here:
import Data.List
import Data.Function

instance NFData a => NFData (VTree a)
instance NFData a => NFData (Direction a)

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

-- Exercise A5

-- Do not modify this datatype
data Dir = U | D | L | R deriving (Eq,Ord,Show,Read)
type Strategy = Dir -> [Dir] -> Maybe Dir 

--Sorting pairs by either ascending x or ascending y
sortingPairs' :: [(Integer,Integer)] -> Int -> [(Integer,Integer)]
sortingPairs' xs b | b == 1 = sortBy (compare `on` fst) xs
                  | b == 2 = sortBy (compare `on` snd) xs

--Finding all coords in the grid that should be a path
coords' :: ((Integer,Integer),(Integer,Integer)) -> [(Integer,Integer)]
coords' ((a,b),(c,d)) | (a == c) && (b == d) = [(a,b)]
                     | (a == c) && (b < d) = [(a,y) | y <- [b..d]]
                     | (a == c) && (b > d) = [(a,y) | y <- [d..b]]
                     | (b == d) && (a < c) = [(x,b) | x <- [a..c]]
                     | (b == d) && (a > c) = [(x,b) | x <- [c..a]]

listOfCoords' :: [((Integer,Integer),(Integer,Integer))] -> [[(Integer,Integer)]]
listOfCoords' = map coords'  

allCoords' :: [((Integer,Integer),(Integer,Integer))] -> [(Integer,Integer)]
allCoords' xs = sortingPairs' (sortingPairs' (concat $ listOfCoords' xs) 2) 1

--Finding all possible directions from current node
checkForSamePath :: [[(Integer,Integer)]] -> (Integer,Integer) -> (Integer,Integer) -> Bool  
checkForSamePath (x:xs) a b | xs == [] = (elem a x) && (elem b x)
                            | (elem a x) && (elem b x) = True 
                            | otherwise = checkForSamePath xs a b 

findUp :: [((Integer,Integer),(Integer,Integer))] -> Maybe (Integer,Integer) -> [[(Integer,Integer)]] -> [Dir]
findUp ms (Just (x,y)) xs | (elem (x,y-1) (allCoords' ms)) && (checkForSamePath xs (x,y) (x,y-1)) = [U]
                          | otherwise = []

findDown :: [((Integer,Integer),(Integer,Integer))] -> Maybe (Integer,Integer) -> [[(Integer,Integer)]] -> [Dir]
findDown ms (Just (x,y)) xs | (elem (x,y+1) (allCoords' ms)) && (checkForSamePath xs (x,y) (x,y+1)) = [D]
                            | otherwise = []

findLeft :: [((Integer,Integer),(Integer,Integer))] -> Maybe (Integer,Integer) -> [[(Integer,Integer)]] -> [Dir]
findLeft ms (Just (x,y)) xs | (elem (x-1,y) (allCoords' ms)) && (checkForSamePath xs (x,y) (x-1,y)) = [L]
                            | otherwise = []

findRight :: [((Integer,Integer),(Integer,Integer))] -> Maybe (Integer,Integer) -> [[(Integer,Integer)]] -> [Dir]
findRight ms (Just (x,y)) xs | (elem (x+1,y) (allCoords' ms)) && (checkForSamePath xs (x,y) (x+1,y)) = [R]
                             | otherwise = []

findAllDir :: [((Integer,Integer),(Integer,Integer))] -> Maybe (Integer,Integer) -> [Dir] 
findAllDir ms (Just (x,y)) | elem (x,y) (allCoords' ms) = findUp ms (Just (x,y)) (listOfCoords' ms) ++ findDown ms (Just (x,y)) (listOfCoords' ms) ++ findLeft ms (Just (x,y)) (listOfCoords' ms) ++ findRight ms (Just (x,y)) (listOfCoords' ms)
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
                                           | elem initial (allCoords' ms) == False = False
                                           | initial == final = run ms strat (move ms strat (Just initial, Just direc), nextDir) final
                                           | otherwise = run ms strat (Just initial, Just direc) final 
                                              where nextDir = strat direc (findAllDir ms (Just initial))

-- Exercise A6
-- Do not modify this datatype
data VTree a = Leaf | Node (VTree a) a Int (VTree a) deriving (Eq,Show,Generic,Generic1)
data Direction a = Lt a Int (VTree a) | Rt a Int (VTree a) deriving (Eq,Show,Generic,Generic1)
type Trail a = [Direction a]
type Zipper a = (VTree a, Trail a)

decide :: (Ord a) => a -> Zipper a -> Zipper a
decide x ((Leaf),trail) = insertValue x ((Leaf),trail) 
decide x ((Node vt1 v n vt2),[]) = insertValue x ((Node vt1 v n vt2),[])
decide x ((Node vt1 v n vt2), trail) | x == v = ((Node vt1 v n vt2), trail)
                                     | null trail = insertValue x ((Node vt1 v n vt2), trail)
                                     | otherwise = help $ moveUp ((Node vt1 v n vt2), trail)
                                          where help ((Node vt3 a b vt4), tr) | x < a && x > v = insertValue x ((Node vt3 a b vt4), tr)
                                                                              | otherwise = decide x ((Node vt3 a b vt4), tr)

insertValue :: (Ord a) => a -> Zipper a -> Zipper a
insertValue x ((Leaf),trail) = (Node Leaf x 1 Leaf,trail)
insertValue x ((Node vt1 v n vt2),trail) | v == x = (Node vt1 v n vt2,trail)
                                    | v < x = insertValue x $ moveRight ((Node vt1 v n vt2),trail)
                                    | v > x = insertValue x $ moveLeft ((Node vt1 v n vt2),trail)

moveUp :: (Ord a) => Zipper a -> Zipper a
moveUp (vt, (Lt v n t : trail)) = (Node (vt) v (n+1) (t), trail)
moveUp (vt, (Rt v n t : trail)) = (Node (t) v (n+1) (vt), trail)

moveLeft :: (Ord a) => Zipper a -> Zipper a 
moveLeft ((Node vt1 v n vt2), ts) = (increment vt1, Lt v n vt2 : ts)

moveRight :: (Ord a) => Zipper a -> Zipper a
moveRight ((Node vt1 v n vt2), ts) = (increment vt2, Rt v n vt1 : ts)

increment :: (Ord a) => VTree a -> VTree a
increment Leaf = Leaf
increment (Node vt1 v n vt2) = (Node vt1 v (n+1) vt2)

contains :: (Ord a) => a -> VTree a -> Bool
contains _ Leaf = False
contains x (Node vt1 v n vt2) | x == v = True
                              | x < v = contains x vt1
                              | x > v = contains x vt2

insertFromCurrentNode :: Ord a => a -> Zipper a -> Zipper a
insertFromCurrentNode item zs | contains item (fst zs) = zs
                              | otherwise = decide item zs 