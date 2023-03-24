{-# LANGUAGE DeriveGeneric #-}
--TEMPLATE FILE FOR COURSEWORK 1 for COMP2209
--Julian Rathke, Oct 2022

--EXERCISE A1 ONLY

--CONTAINS FUNCTION REQIURED FOR COMPILATION AGAINST THE TEST SUITE
--MODIFY THE FUNCTION DEFINITIONS WITH YOUR OWN SOLUTIONS
--IMPORTANT : DO NOT MODIFY ANY FUNCTION TYPES

module Exercises (vigenere) where

-- The following two imports are needed for testing, do not delete

import GHC.Generics (Generic,Generic1)
import Control.DeepSeq
import Data.Char

-- Exercise A1
convert :: (Int -> Int -> Int) -> Char -> Char -> Char
convert op phrase key = cFromAscii ((cToAscii phrase) `op` (cToAscii key))
                        where cToAscii phrase = ord phrase - 65
                              cFromAscii ascii = chr (65 + (ascii `mod` 26))

check :: String -> String
check [] = []
check (a:as) = (filter isLetter [toUpper a]) ++ check as

vigenere :: String -> (String -> String, String -> String)
vigenere a | a == [] = error "Empty String"
           | otherwise = (encr, decr)
             where encr str = zipWith (convert (+)) (check str) (cycle $ check a)
                   decr str = zipWith (convert (-)) (check str) (cycle $ check a)