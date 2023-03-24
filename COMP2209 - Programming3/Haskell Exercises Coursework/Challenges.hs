{-# LANGUAGE DeriveGeneric #-}
-- comp2209 Functional Programming Challenges
-- (c) University of Southampton 2021
-- Skeleton code to be updated with your solutions
-- The dummy functions here simply return an arbitrary value that is usually wrong 

-- DO NOT MODIFY THE FOLLOWING LINES OF CODE
module Challenges (Atoms,Interactions,Pos,EdgePos,Side(..),Marking(..),
                   LamExpr(..),ArithExpr(..),
                   calcBBInteractions,
                   solveBB,
                   prettyPrint,
                   parseArith,
                   churchEnc,
                   innerRedn1,innerArithRedn1,compareArithLam)
where


-- Import standard library and parsing definitions from Hutton 2016, Chapter 13
import Data.Char
import Parsing
import Control.Monad
import Data.List
import Data.Ord
import GHC.Generics (Generic,Generic1)
import Control.DeepSeq

instance NFData ArithExpr
instance NFData LamExpr 
instance NFData Marking
instance NFData Side

-- Challenge 1
-- Calculate Interactions in the Black Box

type Atoms = [ Pos ]
type Interactions = [  ( EdgePos , Marking )  ] 
type Pos = (Int, Int)   -- top left is (1,1) , bottom right is (N,N) where N is size of grid
type EdgePos = ( Side , Int ) -- int range is 1 to N where N is size of grid

data Side = North | East | South | West
            deriving (Show, Eq, Ord, Generic)

data Marking =  Absorb | Reflect | Path EdgePos
                deriving (Show, Eq)

--given current position and direction the ray is coming from, checks for the two diagonals ahead whether there is an atom there or not 
checkForAtomDiagonal :: Pos -> Atoms -> EdgePos -> Atoms
checkForAtomDiagonal _ [] _ = []
checkForAtomDiagonal (a,b) ((x,y):xs) (s,n) | (s == North) && ((a,b) == (x-1,y-1)) = [(x,y)] ++ (checkForAtomDiagonal (a,b) xs (s,n))
                                            | (s == North) && ((a,b) == (x+1,y-1)) = [(x,y)] ++ (checkForAtomDiagonal (a,b) xs (s,n))
                                            | (s == South) && ((a,b) == (x-1,y+1)) = [(x,y)] ++ (checkForAtomDiagonal (a,b) xs (s,n)) 
                                            | (s == South) && ((a,b) == (x+1,y+1)) = [(x,y)] ++ (checkForAtomDiagonal (a,b) xs (s,n))
                                            | (s == East) && ((a,b) == (x+1,y+1)) = [(x,y)] ++ (checkForAtomDiagonal (a,b) xs (s,n))
                                            | (s == East) && ((a,b) == (x+1,y-1)) = [(x,y)] ++ (checkForAtomDiagonal (a,b) xs (s,n))
                                            | (s == West) && ((a,b) == (x-1,y+1)) = [(x,y)] ++ (checkForAtomDiagonal (a,b) xs (s,n))
                                            | (s == West) && ((a,b) == (x-1,y-1)) = [(x,y)] ++ (checkForAtomDiagonal (a,b) xs (s,n))
                                            | otherwise = checkForAtomDiagonal (a,b) xs (s,n)

--given current position and direction the ray is coming from, checks if there is an atom ahead or not 
checkForAtomAhead :: Pos -> Atoms -> EdgePos -> Bool
checkForAtomAhead _ [] _ = False
checkForAtomAhead (a,b) ((x,y):xs) (s,n) | (s == North) && ((a,b) == (x,y-1)) = True
                                         | (s == South) && ((a,b) == (x,y+1)) = True
                                         | (s == East) && ((a,b) == (x+1,y)) = True
                                         | (s == West) && ((a,b) == (x-1,y)) = True
                                         | otherwise = False || checkForAtomAhead (a,b) xs (s,n)

--invert direction the ray is going
reflectDirection :: EdgePos -> EdgePos
reflectDirection (North,x) = (South,x)
reflectDirection (South,x) = (North,x)
reflectDirection (East,x) = (West,x)
reflectDirection (West,x) = (East,x)

--chooses which direction the ray should turn to given the atom locations
whereToMove :: Int -> Pos -> Atoms -> Atoms -> EdgePos -> Marking
whereToMove size (a,b) [(x,y)] atms (s,n) | (s == North) && ((a,b) == (x-1,y-1)) = moveMany size (a,b) atms (East,b)
                                          | (s == North) && ((a,b) == (x+1,y-1)) = moveMany size (a,b) atms (West,b)
                                          | (s == South) && ((a,b) == (x-1,y+1)) = moveMany size (a,b) atms (East,b)
                                          | (s == South) && ((a,b) == (x+1,y+1)) = moveMany size (a,b) atms (West,b)
                                          | (s == East) && ((a,b) == (x+1,y+1)) = moveMany size (a,b) atms (North,a)
                                          | (s == East) && ((a,b) == (x+1,y-1)) = moveMany size (a,b) atms (South,a)
                                          | (s == West) && ((a,b) == (x-1,y+1)) = moveMany size (a,b) atms (North,a)
                                          | (s == West) && ((a,b) == (x-1,y-1)) = moveMany size (a,b) atms (South,a)


moveIntoBB :: Int -> EdgePos -> Pos 
moveIntoBB y (North,x) = (x,0)
moveIntoBB y (South,x) = (x,y+1)
moveIntoBB x (East,y) = (x+1,y)
moveIntoBB x (West,y) = (0,y)

moveOnce :: Pos -> EdgePos -> Pos
moveOnce (x,y) (North,_) = (x,y+1)
moveOnce (x,y) (South,_) = (x,y-1)
moveOnce (x,y) (East,_) = (x-1,y)
moveOnce (x,y) (West,_) = (x+1,y)

--function recursively chooses what to do next, return path if it reaches the edge of the blackBox, absorb if atom ahead, 
--deflect according to atoms in surroundings, otherwise, move forward
moveMany :: Int -> Pos -> Atoms -> EdgePos -> Marking
moveMany size (x,y) a (s,n) | (s == North) && (y == size) = Path (South,x)
                            | (s == South) && (y == 1) = Path (North,x)
                            | (s == East) && (x == 1) = Path (West,y)
                            | (s == West) && (x == size) = Path (East,y)
                            | checkForAtomAhead (x,y) a (s,n) = Absorb
                            | length (checkForAtomDiagonal (x,y) a (s,n)) == 1 = whereToMove size (x,y) (checkForAtomDiagonal (x,y) a (s,n)) a (s,n)
                            | length (checkForAtomDiagonal (x,y) a (s,n)) == 2 = moveMany size (moveOnce (x,y) $ reflectDirection (s,n)) a $ reflectDirection (s,n)
                            | otherwise = moveMany size (moveOnce (x,y) (s,n)) a (s,n)

--starts moving through recursion
move :: Int -> Atoms -> EdgePos -> Marking
move size a ep = moveMany size (moveIntoBB size ep) a ep

calcBBInteractions :: Int -> Atoms -> [EdgePos] -> Interactions
calcBBInteractions 0 _ _ = []
calcBBInteractions _ _ [] = []
calcBBInteractions size atms (x:xs) | checkForAtomAhead (moveIntoBB size x) atms x = (x,Absorb) : (calcBBInteractions size atms xs)
                                    | (checkForAtomDiagonal (moveIntoBB size x) atms x /= []) = (x,Reflect) : (calcBBInteractions size atms xs)
                                    | (Just x == (helper $ move size atms x)) = (x,Reflect) : (calcBBInteractions size atms xs)
                                    | otherwise = (x, move size atms x) : (calcBBInteractions size atms xs)
                                        where helper :: Marking -> Maybe EdgePos
                                              helper Absorb = Nothing
                                              helper Reflect = Nothing
                                              helper (Path x) = Just x

-- Challenge 2
-- Find atoms in a Black Box

{-
goes through every interaction, for every interaction that has a path,
the three coordinates around the start and the end of the path are removed 
-}
filterOut :: Int -> Interactions -> Atoms -> Atoms
filterOut _ [] ys = ys
filterOut size (((x,y), Path (a,b)):xs) atms = filterOut size xs $ removeCoords atms [coord1,coord2,coord3,coord4,coord5,coord6]
                                              where coord1 = (moveOnce (moveIntoBB size (a,b)) (a,b))
                                                    coord2 = (moveOnce (moveIntoBB size (a,b+1)) (a,b))
                                                    coord3 = (moveOnce (moveIntoBB size (a,b-1)) (a,b))
                                                    coord4 = (moveOnce (moveIntoBB size (x,y)) (x,y)) 
                                                    coord5 = (moveOnce (moveIntoBB size (x,y+1)) (x,y))
                                                    coord6 = (moveOnce (moveIntoBB size (x,y-1)) (x,y))
filterOut size (x:xs) ys = filterOut size xs ys                                       

removeCoords :: Atoms -> [Pos] -> Atoms
removeCoords xs [] = xs
removeCoords xs (y:ys) = removeCoords (delete y xs) ys

{-
getting all possible combinations of atoms with a size smaller than 7 for efficiency
-}
getAllPossibleAtoms :: Int -> Interactions -> Atoms
getAllPossibleAtoms size ys = filterOut size ys $ concat [[(x,y) | x <- [1..size]] | y <- [1..size]]

--taken from the internet
powerset :: Atoms -> [Atoms] 
powerset [] = [[]]
powerset (x:xs) = filter (\l -> length l < 7) [x:ps | ps <- powerset xs] ++ powerset xs

getEdgePos :: Interactions -> [EdgePos]
getEdgePos [] = []
getEdgePos ((x,_):xs) = [x] ++ getEdgePos xs


--check for an atom combination if the blackbox is solved properly
satisfiable :: Int -> Interactions -> Atoms -> Bool
satisfiable size xs atms | calcBBInteractions size atms (getEdgePos xs) == xs = True 
                         | otherwise = False 

--check for every atom combination if the blackbox is solved, since haskell is a lazy language, 
--it gives the first answer it finds and stops the recursion
solveBB :: Int -> Interactions -> Atoms
solveBB 0 _ = []
solveBB size xs = solveBBHelper size xs (sortBy (comparing length) $ reverse $ powerset $ getAllPossibleAtoms size xs)
                   where solveBBHelper :: Int -> Interactions -> [Atoms] -> Atoms
                         solveBBHelper size xs [] = error "the blackbox is not solvable"
                         solveBBHelper size xs (ys:yss) | satisfiable size xs ys = ys
                                                        | otherwise = solveBBHelper size xs yss 

-- Challenge 3
-- Pretty Printing Lambda with Alpha-Normalisation 

data LamExpr =  LamApp LamExpr LamExpr  |  LamAbs Int LamExpr  |  LamVar Int 
                deriving (Eq, Show, Read)

--taken from Lecture slides 38
freeVar :: Int -> LamExpr -> Bool
freeVar x (LamVar y) = x == y
freeVar x (LamAbs y e) | x == y = False
freeVar x (LamAbs y e) | x /= y = freeVar x e
freeVar x (LamApp e1 e2) = (freeVar x e1) || (freeVar x e2)

--taken from Lecture slides 38, but slightly modified to suit alpha Conversion instead of Beta reduction
substitution :: LamExpr -> Int -> Int -> LamExpr
substitution (LamVar x) l z | x == l = LamVar z
substitution (LamVar x) l z | x /= l = LamVar x
substitution (LamAbs x y) l z | x /= l && (x /= z) = LamAbs x (substitution y l z)
substitution (LamAbs x y) l z | x /= l && (x == z) = let s = (rename x y) in substitution (LamAbs s (substitution y x s)) l z
substitution (LamAbs x y) l z | x == l = LamAbs x y
substitution (LamApp x y) l z = LamApp (substitution x l z) (substitution y l z) 

--taken from Lecture slides 38
rename :: Int -> LamExpr -> Int
rename x e | freeVar (x+1) e = rename (x+1) e
           | otherwise = (x+1)

alphaConversion :: LamExpr -> LamExpr
alphaConversion (LamVar x) = LamVar x 
alphaConversion (LamAbs x y) = LamAbs new $ alphaConversion $ substitution y x new
                                  where new = head [v | v <- [0..], not $ freeVar v (LamAbs x y)]  
alphaConversion (LamApp x y) = LamApp (alphaConversion x) (alphaConversion y)

--converts a Lambda expression to string
toString :: LamExpr -> String
toString (LamVar x) = "x" ++ show x
toString (LamAbs x y) = "\\x" ++ (show x) ++ " -> " ++ toString y 
toString (LamApp (LamAbs x y) z) = "(" ++ "\\x" ++ (show x) ++ " -> " ++ toString y ++ ") " ++ toString z
toString (LamApp x y) = toString x ++ " " ++ toString y

prettyPrint :: LamExpr -> String
prettyPrint x = toString $ alphaConversion x 

-- Challenge 4 
-- Parsing Arithmetic Expressions

data ArithExpr = Add ArithExpr ArithExpr | Mul ArithExpr ArithExpr 
               | Section ArithExpr  | SecApp ArithExpr ArithExpr | ArithNum Int
    deriving (Show,Eq,Read) 

{-
the parsers follow the following language:

Multiplication ::= Addition "*" Multiplication | Addition
Addition ::= SectionApplication "+" Addition | SectionApplication
SectionApplication ::= "(" "+" Multiplication ")" | Section
Section ::= Num | "(" Multiplication ")"
Num ::= Digits
Digits ::= Digit | Digits Digit
Digit ::= “0” | “1” | “2” | “3” | “4” | “5” | “6” | “7” | “8” | “9”
-}
addition :: Parser ArithExpr
addition = do e1 <- multiplication
              char '+'
              e2 <- addition
              return (Add e1 e2)
           <|> multiplication

multiplication :: Parser ArithExpr
multiplication = do e1 <- secApp
                    char '*'
                    e2 <- multiplication
                    return (Mul e1 e2)
                 <|> secApp

secApp :: Parser ArithExpr
secApp = do char '('
            char '+'
            e1 <- addition
            char ')'
            e2 <- natExpr <|> addition
            return (SecApp (Section e1) e2)
         <|> sec
      
sec :: Parser ArithExpr
sec = natExpr
      <|> do char '('
             e1 <- addition
             char ')'
             return (e1)

natExpr :: Parser ArithExpr
natExpr = do xs <- some digit
             return (ArithNum (read xs))

filterSpaces :: String -> String
filterSpaces xs = filter (\x -> x /= ' ') xs

--returns nothing if negative inputs are given, if the string does not follow the language and if it is empty
parseArith :: String -> Maybe ArithExpr
parseArith xs | elem '-' xs = Nothing
              | (snd (head (parse addition (filterSpaces xs)))) /= [] = Nothing
              | (parse addition (filterSpaces xs)) == [] = Nothing
              | otherwise = Just (fst (head (parse addition (filterSpaces xs))))

-- Challenge 5
-- Church Encoding of arithmetic 

getLambdaFromNumber :: Int -> LamExpr
getLambdaFromNumber x = LamAbs 0 (LamAbs 1 $ (iterate (\l -> LamApp (LamVar 0) l) (LamVar 1)) !! x)

plus :: LamExpr
plus = LamAbs 0 (LamAbs 1 (LamAbs 2 (LamAbs 3 (LamApp (LamApp (LamVar 0) (LamVar 2)) (LamApp (LamApp (LamVar 1)(LamVar 2))(LamVar 3))))))

mult :: LamExpr
mult = LamAbs 0 (LamAbs 1 (LamAbs 2 (LamAbs 3 (LamApp (LamApp (LamVar 0) (LamApp (LamVar 1) (LamVar 2))) (LamVar 3)))))

--following the church encoding rules for lambda expressions, this functions converts arithmetic expressions to lambda expressions
churchEnc :: ArithExpr -> LamExpr 
churchEnc (ArithNum x) = getLambdaFromNumber x
churchEnc (Add x y) = LamApp (LamApp (plus) (churchEnc x)) (churchEnc y)
churchEnc (Mul x y) = LamApp (LamApp (mult) (churchEnc x)) (churchEnc y)
churchEnc (SecApp (Section x) y) = LamApp (LamApp (plus) (churchEnc x)) (churchEnc y)

-- Challenge 6
-- Compare Innermost Reduction for Arithmetic and its Church Encoding

--ARITHMETIC REDUCTION--
--check whether more reductions can be made
checkForArithNum :: ArithExpr -> Bool
checkForArithNum (ArithNum _) = True
checkForArithNum _ = False

{-
find how many reductions are on the lefthand side and how many on the right hand side,
returning always the most reductions possible
-}
getInt :: ArithExpr -> Int
getInt (ArithNum x) = x

getArDepth :: ArithExpr -> Int -> Int
getArDepth (ArithNum x) c = c
getArDepth (Add x y) c | (getArDepth x c) < (getArDepth y c) = getArDepth y (c+1) 
                       | otherwise = getArDepth x (c+1)  
getArDepth (Mul x y) c | (getArDepth x c) < (getArDepth y c) = getArDepth y (c+1)  
                       | otherwise = getArDepth x (c+1)  
getArDepth (SecApp (Section x) y) c | (getArDepth x c) < (getArDepth y c) = getArDepth y (c+1)  
                                    | otherwise = getArDepth x (c+1)  

--check if x or y have more depth, and go the child node accordingly, until a leaf is reached, then calculate it
arithRed :: ArithExpr -> ArithExpr
arithRed (Add x y) | (checkForArithNum x) && (checkForArithNum y) = ArithNum ((getInt x) + (getInt y))
                   | (getArDepth x 0) < (getArDepth y 0) = Add x (arithRed y) 
                   | otherwise = Add (arithRed x) y
arithRed (Mul x y) | (checkForArithNum x) && (checkForArithNum y) = ArithNum ((getInt x) * (getInt y))
                   | (getArDepth x 0) < (getArDepth y 0) = Mul x (arithRed y)
                   | otherwise = Mul (arithRed x) y
arithRed (SecApp (Section x) y) | (checkForArithNum x) && (checkForArithNum y) = ArithNum ((getInt x) + (getInt y))
                                | (getArDepth x 0) < (getArDepth y 0) = SecApp (Section x) (arithRed y)
                                | otherwise = SecApp (Section (arithRed x)) y

--does one reduction, if possible, otherwise returns Nothing
innerArithRedn1 :: ArithExpr -> Maybe ArithExpr
innerArithRedn1 expr | checkForArithNum expr = Nothing
                     | otherwise = Just (arithRed expr)

--counts how many reductions can be done
countArithRed :: Maybe ArithExpr -> Int -> Int
countArithRed (Nothing) x = x-1 
countArithRed (Just expr) x = countArithRed (innerArithRedn1 expr) (x+1)

--LAMBDA REDUCTION--
--taken from Lecture slides 38
subst :: LamExpr -> Int -> LamExpr -> LamExpr
subst (LamVar x) y e | x == y = e
subst (LamVar x) y e | x /= y = LamVar x
subst (LamAbs x e1) y e | x /= y && not (freeVar x e) = LamAbs x (subst e1 y e)
subst (LamAbs x e1) y e | x /= y && (freeVar x e) = let z = (rename x e1) in subst (LamAbs z (subst e1 x (LamVar z))) y e
subst (LamAbs x e1) y e | x == y = LamAbs x e1
subst (LamApp e1 e2) y e = LamApp (subst e1 y e) (subst e2 y e) 

--check whether more reductions can be made
checkForNormalForm :: LamExpr -> Bool 
checkForNormalForm (LamApp (LamAbs x e1) e2) = False
checkForNormalForm (LamApp e1 e2) = (checkForNormalForm e1) && (checkForNormalForm e2) 
checkForNormalForm (LamAbs x e) = checkForNormalForm e
checkForNormalForm _ = True

{-
find how many reductions are on the lefthand side and how many on the right hand side,
returning always the most reductions possible
-}
getLamDepth :: LamExpr -> Int -> Int
getLamDepth (LamVar _) c = c
getLamDepth (LamAbs x e) c = getLamDepth e c
getLamDepth (LamApp (LamAbs x e1) e2) c | (getLamDepth e1 c) < (getLamDepth e2 c) = getLamDepth e2 (c+1)  
                                        | otherwise = getLamDepth e1 (c+1) 
getLamDepth (LamApp e1 e2) c | (getLamDepth e1 c) < (getLamDepth e2 c) = getLamDepth e2 c  
                             | otherwise = getLamDepth e1 c

--check if e1 or e2 have more depth, and go the child node accordingly, until a leaf is reached, then calculate it
lambdaRed :: LamExpr -> LamExpr
lambdaRed (LamVar x) = (LamVar x)
lambdaRed (LamAbs x e) | (getLamDepth e 0 == 0) = (LamAbs x e)
                       | otherwise = LamAbs x (lambdaRed e)
lambdaRed (LamApp e@(LamAbs x e1) e2) | (getLamDepth e1 0 == 0) && (getLamDepth e2 0 == 0) = subst e1 x e2
                                      | (getLamDepth e1 0) < (getLamDepth e2 0) = LamApp e (lambdaRed e2)
                                      | otherwise = LamApp (lambdaRed e) e2
lambdaRed (LamApp e1 e2) | (getLamDepth e1 0) < (getLamDepth e2 0) = LamApp e1 (lambdaRed e2)
                         | otherwise = LamApp (lambdaRed e1) e2

--does one reduction, if possible, otherwise returns Nothing
innerRedn1 :: LamExpr -> Maybe LamExpr
innerRedn1 expr | checkForNormalForm expr = Nothing
                | otherwise = Just (lambdaRed expr)

--counts how many reductions can be done
countLambdaRed :: Maybe LamExpr -> Int -> Int
countLambdaRed (Just expr) x | (getLamDepth expr 0 /= 0) = countLambdaRed (innerRedn1 expr) (x+1)
                             | otherwise = x

--finding number of reductions needed for both ArithRed and LambdaExpr
compareArithLam :: ArithExpr -> (Int,Int)
compareArithLam expr = (countArithRed (Just expr) 0, countLambdaRed (Just (churchEnc expr)) 0)