{-# LANGUAGE DeriveGeneric #-}
--SKELETON FILE FOR HANDIN 2 OF COURSEWORK 1 for COMP2209, 2020
--CONTAINS ALL FUNCTIONS REQIURED FOR COMPILATION AGAINST THE TEST SUITE
--MODIFY THE FUNCTION DEFINITIONS WITH YOUR OWN SOLUTIONS
--IMPORTANT : DO NOT MODIFY ANY FUNCTION TYPES
--Julian Rathke, Oct 2020

module Exercises (Expr(..),toNNF,Binding,Interpretation,consistent,solve,satisfiable,maxSatisfiable) where

-- The following two  imports are needed for testing, do not delete
import GHC.Generics (Generic,Generic1)
import Control.DeepSeq
import Data.List
import Data.Function
import Data.Ord

-- Exercises
data Expr = Var Char | Not Expr | And Expr Expr | Or Expr Expr deriving (Eq, Ord, Show, Read)
type Binding = (Char, Bool)
type Interpretation = [Binding]

--A7--
toNNF :: Expr -> Expr
toNNF (Var c) = (Var c)
toNNF (Not (Var c)) = (Not (Var c))
toNNF (Not (Not x)) = toNNF x
toNNF (Not (And x y)) = Or (toNNF (Not x)) (toNNF (Not y))
toNNF (Not (Or x y)) = And (toNNF (Not x)) (toNNF (Not y))
toNNF (And x y) = And (toNNF x) (toNNF y)
toNNF (Or x y) = Or (toNNF x) (toNNF y)

--A8--
consistent :: Interpretation -> Bool
consistent [] = True
consistent (x:xs) = (check (x:xs) x) && consistent xs
                     where check :: Interpretation -> Binding -> Bool
                           check [] _ = True
                           check ((c,b):ys) (c1,b1) | (c == c1) && (b /= b1) = False
                                                    | otherwise = check ys (c1,b1)

checkForConsistency :: [Interpretation] -> [Interpretation]
checkForConsistency [] = []
checkForConsistency (x:xs) | consistent x = [x] ++ checkForConsistency xs
                           | otherwise = checkForConsistency xs

sortByLength :: [Interpretation] -> [Interpretation]
sortByLength xs = sortBy (comparing length) xs

removeSupersets :: [Interpretation] -> [Interpretation]
removeSupersets [] = []
removeSupersets (xs:xss) | (isSuperset xs xss []) == xss =  [xs] ++ removeSupersets xss
                         | otherwise = [xs] ++ removeSupersets (isSuperset xs xss [])

isSuperset :: Interpretation -> [Interpretation] -> [Interpretation] -> [Interpretation]
isSuperset _ [] acc = acc
isSuperset x (y:ys) acc | (x `intersect` y) == x = isSuperset x (delete y ys) (acc)
                        | otherwise = isSuperset x ys (acc ++ [y])

dup :: [Interpretation] -> [Interpretation] -> [Interpretation]
dup [] _ = []
dup _ [] = []
dup (a:as) (b:bs) = nub $ [a ++ b] ++ dup as (b:bs) ++ dup (a:as) (bs)

sortingPairs :: Int -> Interpretation ->  Interpretation
sortingPairs b xs | b == 1 = sortBy (compare `on` fst) xs
                  | b == 2 = sortBy (compare `on` snd) xs

solve :: Expr -> [Interpretation]
solve expr = removeSupersets . sortByLength $ map (sortingPairs 1) (map nub $ checkForConsistency . helper $ toNNF expr) 
               where helper :: Expr -> [Interpretation]
                     helper (Var c) = [[(c,True)]]
                     helper (Not (Var c)) = [[(c,False)]]
                     helper (And (Or x y) (Or w z)) = (dup (helper x) (helper w)) ++ (dup (helper x) (helper z)) ++ (dup (helper y) (helper w)) ++ (dup (helper y) (helper w))
                     helper (And (Or x y) z) = (dup (helper x) (helper z)) ++ (dup (helper y) (helper z))
                     helper (And x (Or y z)) = (dup (helper x) (helper y)) ++ (dup (helper x) (helper z))
                     helper (Or x y) = (helper x) ++ (helper y) 
                     helper (And x y) = dup (helper x) (helper y)

wrapAnd :: [Expr] -> Expr
wrapAnd [x] = x
wrapAnd (x:xs) = And x (wrapAnd xs)

satisfiable :: [Expr] -> Bool
satisfiable [] = True
satisfiable xs | (solve $ wrapAnd xs) == [] = False
               | otherwise = True 

--A9--
powerset :: [Expr] -> [[Expr]]
powerset [] = [[]]
powerset (x:xs) = [x:ps | ps <- powerset xs] ++ powerset xs

maximal :: [[Expr]] -> [[Expr]]
maximal [] = []
maximal [[]] = [[]]
maximal [[x]] = [[x]]
maximal (x:y:xs) | (length x) == (length y) = [x] ++ maximal (y:xs)
                 | otherwise = [x]                

sortByLength2 :: [[Expr]] -> [[Expr]]
sortByLength2 [] = [[]]
sortByLength2 xs = sortBy (flip $ comparing length) xs

helper2 :: [[Expr]] -> [[Expr]]
helper2 [] = []
helper2 (x:xs) | satisfiable x = [x] ++ helper2 xs
               | otherwise = helper2 xs

maxSatisfiable :: [Expr] -> [[Expr]]
maxSatisfiable [] = [[]]
maxSatisfiable xs = maximal . sortByLength2 . helper2 $ delete ([]) $ powerset xs