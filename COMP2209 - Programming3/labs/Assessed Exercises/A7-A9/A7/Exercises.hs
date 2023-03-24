{-# LANGUAGE DeriveGeneric #-}
--SKELETON FILE FOR HANDIN 2 OF COURSEWORK 1 for COMP2209, 2020
--CONTAINS ALL FUNCTIONS REQIURED FOR COMPILATION AGAINST THE TEST SUITE
--MODIFY THE FUNCTION DEFINITIONS WITH YOUR OWN SOLUTIONS
--IMPORTANT : DO NOT MODIFY ANY FUNCTION TYPES
--Julian Rathke, Oct 2020

module Exercises (Expr(..),toNNF) where

-- The following two  imports are needed for testing, do not delete
import GHC.Generics (Generic,Generic1)
import Control.DeepSeq

-- Exercise A7
data Expr = Var Char | Not Expr | And Expr Expr | Or Expr Expr deriving (Eq, Ord, Show, Read)

-- Define toNNF here:
toNNF :: Expr -> Expr
toNNF (Var c) = (Var c)
toNNF (Not (Var c)) = (Not (Var c))
toNNF (Not (Not x)) = toNNF x
toNNF (Not (And x y)) = Or (toNNF (Not x)) (toNNF (Not y))
toNNF (Not (Or x y)) = And (toNNF (Not x)) (toNNF (Not y))
toNNF (And x y) = And (toNNF x) (toNNF y)
toNNF (Or x y) = Or (toNNF x) (toNNF y)