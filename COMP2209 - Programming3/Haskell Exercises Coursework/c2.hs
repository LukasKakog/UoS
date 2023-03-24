-- Challenge 2
-- Find atoms in a Black Box

import Data.List
import Data.Ord
import Data.Function

type Atoms = [Pos]
type Interactions = [(EdgePos,Marking)] 
type Pos = (Int, Int) -- (column, row) top left is (1,1) , bottom right is (N,N) where N is size of grid
type EdgePos = (Side,Int) -- int range is 1 to N where N is size of grid
data Side = North | East | South | West deriving (Eq, Ord, Show, Read)
data Marking =  Absorb | Reflect | Path EdgePos deriving (Show, Eq)

{-
--Sorting atoms by descending score
sortingAtoms :: [(Pos,Int)] -> [(Pos,Int)]
sortingAtoms xs = reverse $ sortBy (compare `on` snd) xs

filterOut :: Int -> Interactions -> Atoms -> Atoms
filterOut _ [] ys = ys
filterOut size (((x,y), Path (a,b)):xs) atms = filterOut size xs $ delete (moveOnce (moveIntoBB size (a,b+1)) (a,b)) $ delete (moveOnce (moveIntoBB size (a,b-1)) (a,b)) $ delete (moveOnce (moveIntoBB size (a,b)) (a,b)) $ delete (moveOnce (moveIntoBB size (x,y+1)) (x,y)) $ delete (moveOnce (moveIntoBB size (x,y-1)) (x,y)) $ delete (moveOnce (moveIntoBB size (x,y)) (x,y)) as
filterOut size (x:xs) ys = filterOut size xs ys                                       

getAllPossibleAtoms :: Int -> Interactions -> Atoms
getAllPossibleAtoms size ys = filterOut size ys $ concat [[(x,y) | x <- [1..size]] | y <- [1..size]]

getEdgePos :: Interactions -> [EdgePos]
getEdgePos [] = []
getEdgePos ((x,_):xs) = [x] ++ getEdgePos xs

satisfiable :: Int -> Atoms -> Interactions -> Bool
satisfiable size atms xs | calcBBInteractions size atms (getEdgePos xs) == xs = True
                       | otherwise = False

calculateScore :: Int -> Interactions -> Int -> Pos -> (Pos,Int)
calculateScore size [] c atom = (atom,c)
calculateScore size (x:xs) c atom | satisfiable size [atom] [x] = calculateScore size xs (c+1) atom 
                                  | otherwise = calculateScore size xs c atom 

calculateAllScores :: Int -> Atoms -> Interactions -> [(Pos,Int)]
calculateAllScores size xs ys = sortingAtoms $ map (calculateScore size ys 0) xs

solve :: Int -> Interactions -> Atoms -> Atoms
solve size xs ys | calcBBInteractions size ys (getEdgePos xs) == xs = ys
                 | otherwise = 
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

getAllPossibleAtoms :: Int -> Interactions -> Atoms
getAllPossibleAtoms size ys = filterOut size ys $ concat [[(x,y) | x <- [1..size]] | y <- [1..size]]

powerset :: Atoms -> [Atoms] 
powerset [] = [[]]
powerset (x:xs) = filter (\l -> length l < 7) [x:ps | ps <- powerset xs] ++ powerset xs

getEdgePos :: Interactions -> [EdgePos]
getEdgePos [] = []
getEdgePos ((x,_):xs) = [x] ++ getEdgePos xs

satisfiable :: Int -> Interactions -> Atoms -> Bool
satisfiable size xs atms | calcBBInteractions size atms (getEdgePos xs) == xs = True 
                         | otherwise = False 

solveBB :: Int -> Interactions -> Atoms
solveBB 0 _ = []
solveBB size xs = solveBBHelper size xs (sortBy (comparing length) $ reverse $ powerset $ getAllPossibleAtoms size xs)
                   where solveBBHelper :: Int -> Interactions -> [Atoms] -> Atoms
                         solveBBHelper size xs [] = error "the blackbox is not solvable"
                         solveBBHelper size xs (ys:yss) | satisfiable size xs ys = ys
                                                        | otherwise = solveBBHelper size xs yss 

--CHANGE NOTHING BELOW HERE (C1)--
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

checkForAtomAhead :: Pos -> Atoms -> EdgePos -> Bool
checkForAtomAhead _ [] _ = False
checkForAtomAhead (a,b) ((x,y):xs) (s,n) | (s == North) && ((a,b) == (x,y-1)) = True
                                         | (s == South) && ((a,b) == (x,y+1)) = True
                                         | (s == East) && ((a,b) == (x+1,y)) = True
                                         | (s == West) && ((a,b) == (x-1,y)) = True
                                         | otherwise = False || checkForAtomAhead (a,b) xs (s,n)

reflectDirection :: EdgePos -> EdgePos
reflectDirection (North,x) = (South,x)
reflectDirection (South,x) = (North,x)
reflectDirection (East,x) = (West,x)
reflectDirection (West,x) = (East,x)

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

moveMany :: Int -> Pos -> Atoms -> EdgePos -> Marking
moveMany size (x,y) a (s,n) | (s == North) && (y == size) = Path (South,x)
                            | (s == South) && (y == 1) = Path (North,x)
                            | (s == East) && (x == 1) = Path (West,y)
                            | (s == West) && (x == size) = Path (East,y)
                            | checkForAtomAhead (x,y) a (s,n) = Absorb                            
                            | length (checkForAtomDiagonal (x,y) a (s,n)) == 1 = whereToMove size (x,y) (checkForAtomDiagonal (x,y) a (s,n)) a (s,n)
                            | length (checkForAtomDiagonal (x,y) a (s,n)) == 2 = moveMany size (moveOnce (x,y) $ reflectDirection (s,n)) a $ reflectDirection (s,n)
                            | otherwise = moveMany size (moveOnce (x,y) (s,n)) a (s,n)

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

