import System.Environment
import System.IO
import Debug.Trace
import System.FilePath (takeExtension, dropExtension)
import Tokens
import Grammar
import TileType
import Data.List (transpose, filter)
import Data.Maybe (isNothing, fromJust)

-- TileT and TileGroup were added to make readability easier
type TileT = [[Int]]
type TileGroup = [TileT]

-- Environment is used to store all the variables used by the program and their respective values
type Environment = [ (String,Exp) ]

-- .*Hole Frames are user to store the environment and sometimes 
-- an expression which is to be calculated in a later step of the evaluation
-- Get.* Frames store an already calculated value to be used in a later step of evaluation
data Frame = AddHole Exp Environment 
            | GetAdd Exp 
            | MultHole Exp Environment 
            | GetMult Exp 
            | MinusHole Exp Environment 
            | GetMinus Exp 
            | DivHole Exp Environment 
            | GetDiv Exp 
            | EqualHole Exp Environment 
            | GetEqual Exp 
            | LessHole Exp Environment 
            | GetLess Exp 
            | AndHole Exp Environment 
            | GetAnd Exp 
            | OrHole Exp Environment 
            | GetOr Exp 
            | NotHole Environment
            | IfHole Exp Exp Environment 
            | ElseIfHole Exp Exp Environment 
            | ForHole Exp Environment 
            | AssignHole String Environment
            | AccessorHole String 
            | HashesHole Exp 
            | HashHole
            | SemicHole Environment
            | SemisHole Exp
            | SubHole Exp Exp Exp Environment 
            | SubXHole Exp Exp Exp Environment
            | SubYHole Exp Exp Exp Environment
            | SubSizeHole Exp Exp Exp Environment
            | ConjunctionHole Exp Environment 
            | GetConjunction Exp
            | NegationHole Environment 
            | RotateHole Environment 
            | SuperHole Exp Environment 
            | GetSuper Exp Environment
            | NumColHole Environment 
            | NumRowHole Environment 
            | FlipXHole Environment 
            | FlipYHole Environment 
            | JoinHole Environment 
            | StackHole Environment
            | BlankHole Exp
            | GetBlank Exp
            deriving (Show, Eq, Read)

-- A kontinuation stores a list of all the frames that still need to be calculated
type Kontinuation = [ Frame ]
-- A configuration stores the current state of the CEK machine
type Configuration = (Exp,Environment,Kontinuation)


-- Main function
main = do args <- getArgs
          reader $ head args 

-- Reads the code and executes it
reader :: FilePath -> IO ()
reader path | takeExtension path /= ".tsl" = error $ "The file given does not have the correct type: " ++ path
            | otherwise = do x <- readFile path
                             let tokens = alexScanTokens x
                             let parsed = parseCalc tokens
                             environment <- fetch parsed
                             let altparsed = cleaner $ parsed
                             let _ = typeOf [] parsed
                             let output = evalLoop altparsed environment
                             putStr output


-- Gets rid of all the Imports
cleaner :: Exp -> Exp
cleaner (Imports x y) = cleaner y
cleaner e = e

-- Fetches the import files to add into the Environment
fetch :: Exp -> IO Environment
fetch exp = envCreator [] $ check $ filter (not.null) $ helper exp
            where helper :: Exp -> [String]
                  helper (Imports x y) = x : helper y
                  helper _ = []
                  check :: [FilePath] -> [FilePath]
                  check [] = []
                  check filepaths | all (\x -> takeExtension x == ".tl") filepaths = filepaths
                                  | otherwise = error "Cannot import a file that is not of type \".tl\"."

-- Creates the environment for IO
envCreator :: Environment -> [FilePath] -> IO Environment
envCreator acc [] = do return acc 
envCreator acc xs = do x <- readFile $ head xs
                       if (not.null $ xs) then
                           let j = (dropExtension $ head xs, tileTheCreator x)
                           in envCreator (j:acc) $ tail xs
                       else 
                           return acc

-- Create a tile from a string input
tileTheCreator :: String -> Exp
tileTheCreator [] = error "Empty input on tile creator."
tileTheCreator input = convertBack $ igor [] input
            where igor :: [Int] -> String -> TileT
                  igor acc [] = [acc]
                  igor acc (x:xs) | x == '\n' = acc : igor [] xs
                                  | x == '0' = igor (acc ++ [0]) xs
                                  | x == '1' = igor (acc ++ [1]) xs
                                  | otherwise = error "Tile contains inputs other than 0 or 1."


-- printTile function, only prints tiles of size N x N
printTile :: Exp -> Environment -> String
printTile tile@(Tile e) _ = expToString tile
printTile (Var str) env = expToString $ findExp str env
printTile _ _ = error "Cannot print something that is not a tile. Check the print function input."

-- helper for printing a tile
expToString :: Exp -> String
expToString x | numCol x == numRow x = unlines $ toString $ convert x
              | otherwise = error $ "Cannot print tile, not of size N x N. Size: Columns = " ++ (show $ numCol x) ++ ", Rows = " ++ (show $ numRow x)
                where toString :: TileT -> [String]
                      toString [] = []
                      toString (xs:xss) = (filter (\i -> i /= ' ') $ unwords [show x | x <- xs]) : toString xss

-- create function, checks whether tile has the right size and returns the tile
create :: Exp -> Exp
create newtile = checker $ convert newtile
        where size = length $ head $ convert newtile
              checker :: TileT -> Exp
              checker [] = newtile
              checker (xs:xss) | length xs == size = checker xss
                               | otherwise = error "Column size is not consistent, cannot create tile."

-- blank function, creates a tile with only 0s
blank :: Int -> Int -> Exp
blank 0 _ = convertBack [[]]
blank _ 0 = convertBack [[]]
blank x y = convertBack [[0 | i <- [1..x]]|j <- [1..y]]

-- flipx function, flips the tile on the x-axis
flipx :: Exp -> Exp
flipx tile = convertBack $ reverse $ convert tile

-- flipy function, flips the tile on the y-axis
flipy :: Exp -> Exp
flipy tile = convertBack [reverse x | x <- convert tile]

-- numCol function, checks whether tile has the right size and returns the number of columns
numCol :: Exp -> Int
numCol tile = checker $ convert tile 
        where size = length $ head $ convert tile
              checker :: TileT -> Int
              checker [] = size
              checker (xs:xss) | length xs == size = checker xss
                               | otherwise = error "Column size is not consistent."

-- numRow function, returns the number of rows
numRow :: Exp -> Int
numRow tile = length $ convert tile

-- rotate function, rotates the tile 90 degrees clock-wise
rotate :: Exp -> Exp
rotate tile = convertBack $ rota $ convert tile
        where rota :: TileT -> TileT
              rota = transpose . reverse
 
-- subtile function, takes a pair of coordinates which will be the top left corner of the new tile and its size
subtile :: Exp -> Int -> Int -> Int -> Exp
subtile mainTile x y s | (numCol mainTile) - x < s = error $ "Cannot extract a subtile of size " ++ show s
                       | (numRow mainTile) - y < s = error $ "Cannot extract a subtile of size " ++ show s
                       | otherwise = convertBack $ extract 0 0 []
                       where extract :: Int -> Int -> [Int] -> TileT
                             extract col row acc | row == s = []
                                                 | col == s = (reverse acc) : extract 0 (row + 1) []
                                                 | otherwise = extract (col + 1) row ((((convert mainTile) !! (y + row)) !! (x + col)):acc)

-- conj function, takes two tiles and returns its boolean conjunction equivalent
conj :: Exp -> Exp -> Exp
conj tile1 tile2 | numRow tile1 == numRow tile2 && numCol tile1 == numCol tile2 = convertBack $ helper tl1 tl2
                 | otherwise = error $ "The sizes of the two tiles do not match: " ++ (show $ numCol tile1) ++
                                   " x " ++ (show $ numRow tile1) ++ " and " ++ (show $ numCol tile2) ++ " x " ++ (show $ numRow tile2)
                 where tl1 = convert tile1
                       tl2 = convert tile2
                       helper :: TileT -> TileT -> TileT
                       helper [] [] = []
                       helper (t1:t1s) (t2:t2s) = conjunction t1 t2 : helper t1s t2s
                       match :: (Int,Int) -> Int
                       match (a,b) | a == 1 && b == 1 = 1
                                   | otherwise = 0
                       conjunction :: [Int] -> [Int] -> [Int]
                       conjunction xs ys = map (match) $ zip xs ys  

-- neg function, takes a tile and returns its boolean negation equivalent
neg :: Exp -> Exp
neg tile = convertBack [[negateTl i | i <- j]| j <- convert tile]
    where negateTl :: Int -> Int
          negateTl n | n == 0 = 1
                     | n == 1 = 0

-- supersize function, takes a tile and multiplies its size by the scale factor
supersize :: Exp -> Int -> Exp
supersize tyler s = convertBack $ helper (convert tyler)
            where copy :: [Int] -> [Int]
                  copy [] = []
                  copy (x:xs) = [x | j <- [1..s]] ++ copy xs
                  helper :: TileT -> TileT
                  helper [] = []
                  helper (t:tile) = [copy t | i <- [1..s]] ++ helper tile

-- Helper tile constructor function used in stack and join functions 
tConstructor :: (TileT -> TileGroup -> TileT) -> TileGroup -> TileT
tConstructor _ [] = [[]]
tConstructor f tg | head tg == [[]] = tConstructor f (tail tg)
                  | otherwise = f (head tg) (tail tg)

-- stack function, takes a TileGroup, stacks all of them in a column, and returns it as a single tile
stack :: Exp -> Environment -> Exp
stack group env = convertBack $ tConstructor helper $ tgConverter group env
    where helper :: TileT -> TileGroup -> TileT
          helper acc [] = acc
          helper acc (r:rest) | r == [[]] = helper acc rest
                              | numCol (convertBack acc) == numCol (convertBack r) = helper (acc ++ r) rest
                              | otherwise = error $ "Cannot stack two tiles of different column number:  " ++ 
                                        (show acc) ++ " and " ++ (show r) 

-- join function, takes a TileGroup, joins all of them in a row, and returns it as a single tile
join :: Exp -> Environment -> Exp
join group env = convertBack $ tConstructor helper $ tgConverter group env
           where helper :: TileT -> TileGroup -> TileT
                 helper acc [] = acc
                 helper acc (r:rest) | r == [[]] = helper acc rest
                                     | numRow (convertBack acc) == numRow (convertBack r) = helper (joiner acc r) rest
                                     | otherwise = error $ "Cannot join two tiles of different row number: " ++ 
                                                (show acc) ++ " and " ++ (show r)
                 joiner :: TileT -> TileT -> TileT
                 joiner [] [] = []
                 joiner (r1:tile1) (r2:tile2) = (r1 ++ r2) : joiner tile1 tile2


-- for-Loop function, used to simulate a for loop, uses configurations so the environment and frames can be accessed
forLoop :: (Configuration, Exp) -> Int -> Configuration
forLoop (cnfg, stdrd) 0 = cnfg
forLoop (cnfg@(e1,env,k), stdrd) counter | counter < 0 = error "For-loop counter cannot be negative."
                                         | counter == 1 = case decideEval cnfg of 
                                                            (e2,env2,k2) -> forLoop ((e2,env2,k2),stdrd) (counter - 1)
                                         | otherwise = case decideEval cnfg of 
                                                            (e2,env2,k2) -> forLoop ((stdrd,env2,k2),stdrd) (counter - 1)
                                              where decideEval :: Configuration -> Configuration
                                                    decideEval ((Hashes ex1 ex2), env3, k3) = forLoopEval (ex1, env3, (HashesHole ex2):k3)
                                                    decideEval ((Hash ex1), env3, k3) = forLoopEval (ex1, env3, (HashHole):k3)
                                                    forLoopEval :: Configuration -> Configuration
                                                    forLoopEval (ex1, env3, (HashesHole ex2):k3) | isValue ex1 = decideEval (ex2, env3, k3)
                                                    forLoopEval (ex1, env3, (HashHole):k3) | isValue ex1 = (ex1, env3, k3)
                                                    forLoopEval cnf = forLoopEval $ eval1 cnf

-- && function, boolean And for our Exp boolean type
bAND :: Exp -> Exp -> Exp
bAND BTrue BTrue = BTrue
bAND _ _ = BFalse

-- || function, boolean Or for our Exp boolean type
bOR :: Exp -> Exp -> Exp 
bOR BFalse BFalse = BFalse
bOR _ _ = BTrue

-- not function, boolean Not for our Exp boolean type
bNOT :: Exp -> Exp
bNOT BTrue = BFalse
bNOT BFalse = BTrue

-- accessor for TGroup, uses a loop to traverse through the list until the required index
-- finds the expression that matches that variable in the environment and returns it
access :: Exp -> Int -> Environment -> Exp
access l n env = loop l 0 
            where loop :: Exp -> Int -> Exp
                  loop (Single a) i | i == n = (findExp a env)
                                    | otherwise = error "Index out of bounds: TileGroup Accessor."
                  loop (List a b) i | i == n = (findExp a env)
                                    | otherwise = loop b (i+1)


-- Used to convert from Exp to TileT
convert :: Exp -> TileT 
convert (Tile ex1) = helper ex1
            where helper2 :: Exp -> [Int]
                  helper2 (Block a)    = [a]
                  helper2 (Blocks a b) = [a] ++ (helper2 b) 
                  helper :: Exp -> TileT
                  helper  (Row a)    = [helper2 a]
                  helper  (Rows a b) = helper2 a : helper b
convert _ = error "Illegal input, not a tile expression."

-- Used to convert from TileT to the Exp equivalent of a Tile
convertBack :: TileT -> Exp
convertBack [] = error "Empty input, cannot convert tile to Exp."
convertBack tile = Tile $ helper tile
            where helper :: TileT -> Exp
                  helper [x] = Row $ helper2 x
                  helper (x:xs)  = Rows (helper2 x) $ helper xs
                  helper2 :: [Int] -> Exp
                  helper2 [y] = Block y
                  helper2 (y:ys) = Blocks y $ helper2 ys

-- function to find an expression given a variable in the environment
findExp :: String -> Environment -> Exp
findExp x env | isNothing $ lookup x env = error $ "Unbound variable: " ++ x
              | otherwise = fromJust $ lookup x env 

-- function that converts a TGroup expression into a TileGroup type
tgConverter :: Exp -> Environment -> TileGroup
tgConverter _ [] = error "No bound variables found." 
tgConverter (Single str) env = [convert $ findExp str env]
tgConverter (List str e1) env = (convert $ findExp str env) : tgConverter e1 env
tgConverter _ _ = error "The given expression is not a TileGroup." 

-- VVV CEK Machine VVV --

-- Function to update an environment with a new binding
update :: Environment -> String -> Exp -> Environment
update env x e | isNothing $ lookup x env = (x,e) : env
               | otherwise = (x, e) : (remove x env)
                where remove :: String -> Environment -> Environment
                      remove key env = filter ((key /=) . fst) env

-- Function to check whether expression has reached a finalised form
isValue :: Exp -> Bool
isValue (BTrue) = True
isValue (BFalse) = True
isValue (Int _) = True
isValue (Tile _) = True
isValue (TGroup _ _) = True
isValue _ = False

-- Function that performs the small step reduction once
eval1 :: Configuration -> Configuration
-- Rule No. 1
eval1 (Var x, env, k) =  (e', env, k) 
      where (e') = (findExp x env)
-- Rule No. 2
eval1 ((Add e1 e2), env, k) =  (e1, env, (AddHole e2 env):k)
eval1 (n, env, (AddHole e2 env2):k) | isValue n =  (e2, env2, (GetAdd n):k)
eval1 ((Int n), env, (GetAdd (Int m)):k) =  (Int (n+m), env, k)
-- Rule No. 3
eval1 ((Minus e1 e2), env, k) =  (e1, env, (MinusHole e2 env):k)
eval1 (n, env, (MinusHole e2 env2):k) | isValue n =  (e2, env2, (GetMinus n):k)
eval1 ((Int n), env, (GetMinus (Int m)):k) =  (Int (m-n), env, k)  
-- Rule No. 4
eval1 ((Mult e1 e2), env, k) =  (e1, env, (MultHole e2 env):k)
eval1 (n, env, (MultHole e2 env2):k) | isValue n =  (e2, env2, (GetMult n):k)
eval1 ((Int n), env, (GetMult (Int m)):k) =  (Int (n*m), env, k) 
-- Rule No. 5
eval1 ((Div e1 e2), env, k) =  (e1, env, (DivHole e2 env):k)
eval1 (n, env, (DivHole e2 env2):k) | isValue n =  (e2, env2, (GetDiv n):k)
eval1 ((Int n), env, (GetDiv (Int m)):k) =  (Int (m `div` n), env, k)  
-- Rule No. 6
eval1 ((Less e1 e2), env, k) =  (e1, env, (LessHole e2 env):k)
eval1 (n, env, (LessHole m env2):k) | isValue n =  (m, env2, (GetLess n):k)
eval1 ((Int n), env, (GetLess (Int m)):k) | (m < n) =  (BTrue, env, k)
eval1 ((Int n), env, (GetLess (Int m)):k) | (m >= n) =  (BFalse, env, k)
-- Rule No. 7
eval1 ((Equality e1 e2),env,k) =  (e1, env, (EqualHole e2 env):k)
eval1 (n, env, (EqualHole m env2):k) | isValue n =  (m, env2, (GetEqual n):k)
eval1 ((Int n), env, (GetEqual (Int m)):k) | (n == m) =  (BTrue, env, k)
eval1 ((Int n), env, (GetEqual (Int m)):k) | (n /= m) =  (BFalse, env, k)
-- Rule No. 8
eval1 ((If e1 e2 e3), env, k) =  (e1, env, (IfHole e2 e3 env):k)
eval1 (BTrue, env, (IfHole e2 e3 env2):k) =  (e2, env2, k)
eval1 (BFalse, env, (IfHole e2 e3 env2):k) =  (e3, env2, k)
-- Rule No. 9
eval1 ((ElseIf e1 e2 e3), env, k) =  (e1, env, (ElseIfHole e2 e3 env):k)
eval1 (BTrue, env, (ElseIfHole e2 e3 env2):k) =  (e2, env2, k)
eval1 (BFalse, env, (ElseIfHole e2 e3 env2):k) =  (e3, env2, k)
-- Rule No. 10
eval1 ((Else e1), env, k) =  (e1, env, k)
-- Rule No. 11 
eval1 ((For e1 e2), env, k) =  (e1, env, (ForHole e2 env):k)
eval1 ((Int x), env, (ForHole e2 env2):k) = forLoop ((e2, env2, k), e2) x
-- Rule No. 12 
eval1 ((Assign str e1), env, k) =  (e1, env, (AssignHole str env):k)
eval1 (e1, env, (AssignHole str env2):k) | isValue e1 =  (Var str, (update env2 str e1), k)
-- Rule No. 13
eval1 ((TGroup str e1), env, k) =  (e1, (update env str e1), k)
-- Rule No. 14 
eval1 ((Accessor str x), env, k) = (x, env, (AccessorHole str):k)
eval1 ((Int x), env, (AccessorHole str):k) = (e, env, k)
      where e = access (findExp str env) x env
-- Rule No. 15 
eval1 ((Blank e1 e2), env, k) = (e1, env, (BlankHole e2):k) 
eval1 (e1, env, (BlankHole e2):k) | isValue e1 = (e2, env, (GetBlank e1):k) 
eval1 ((Int n), env, (GetBlank (Int m)):k) = (blank m n, env, k)
-- Rule No. 16
eval1 ((NumRow e1), env, k) =  (e1, env, (NumRowHole env):k)
eval1 (e1, env, (NumRowHole env2):k) | isValue e1 =  (Int (numRow e1), env2, k) 
-- Rule No. 17
eval1 ((NumCol e1), env, k) =  (e1, env, (NumColHole env):k)
eval1 (e1, env, (NumColHole env2):k) | isValue e1 =  (Int (numCol e1), env2, k) 
-- Rule No. 18
eval1 ((FlipX e1), env, k) =  (e1, env, (FlipXHole env):k)
eval1 (e1, env, (FlipXHole env2):k) | isValue e1 =  (flipx e1, env2, k)
-- Rule No. 19
eval1 ((FlipY e1), env, k) =  (e1, env, (FlipYHole env):k)
eval1 (e1, env, (FlipYHole env2):k) | isValue e1 =  (flipy e1, env2, k)
-- Rule No. 20 
eval1 ((Join e1), env, k) =  (e1, env, (JoinHole env):k)
eval1 (e1, env, (JoinHole env2):k) =  (join e1 env, env2, k)
-- Rule No. 21 
eval1 ((Stack e1), env, k) =  (e1, env, (StackHole env):k)
eval1 (e1, env, (StackHole env2):k) =  (stack e1 env, env2, k)
-- Rule No. 22
eval1 ((Rotate e1), env, k) =  (e1, env, (RotateHole env):k)
eval1 (e1, env, (RotateHole env2):k) | isValue e1 =  (rotate e1, env2, k)
-- Rule No. 23
eval1 ((Sub e1 x y size), env, k) =  (e1, env, (SubHole x y size env):k)
eval1 (e1, env, (SubHole x y size env2):k) | isValue e1 = (x, env, (SubXHole e1 y size env2):k)
eval1 (x, env, (SubXHole e1 y size env2):k) | isValue x = (y, env, (SubYHole e1 x size env2):k)
eval1 (y, env, (SubYHole e1 x size env2):k) | isValue y = (size, env, (SubSizeHole e1 x y env2):k)
eval1 ((Int s), env, (SubSizeHole e1 (Int x) (Int y) env2):k) | isValue e1 = (subtile e1 x y s, env2, k)
-- Rule No. 24
eval1 ((Super e1 scale), env, k) =  (e1, env, (SuperHole scale env):k)
eval1 (e1, env, (SuperHole scale env2):k) | isValue e1 = (scale, env, (GetSuper e1 env2):k)
eval1 ((Int s), env, (GetSuper e1 env2):k) = (supersize e1 s, env2, k)
-- Rule No. 25
eval1 ((Conjunction e1 e2), env, k) =  (e1, env, (ConjunctionHole e2 env):k)
eval1 (e1, env, (ConjunctionHole e2 env2):k) | isValue e1 =  (e2, env2, (GetConjunction e1):k)
eval1 (e1, env, (GetConjunction e2):k) | isValue e1 =  (conj e1 e2, [], k)
-- Rule No. 26
eval1 ((Negation e1), env, k) =  (e1, env, (NegationHole env):k)
eval1 (e1, env, (NegationHole env2):k) | isValue e1 =  (neg e1, env2, k)
-- Rule No. 27
eval1 ((And e1 e2), env, k) =  (e1,env, (AndHole e2 env):k)
eval1 (n, env, (AndHole m env2):k) | isValue n =  (m, env2, (GetAnd n):k)
eval1 (n, env, (GetAnd m):k) | isValue n =  (bAND n m, env, k)
-- Rule No. 28
eval1 ((Or e1 e2), env, k) =  (e1, env, (OrHole e2 env):k)
eval1 (n, env, (OrHole m env2):k) | isValue n =  (m, env2, (GetOr n):k)
eval1 (n, env, (GetOr m):k) | isValue n =  (bOR n m, env, k) 
-- Rule No. 29
eval1 ((Not e1), env, k) =  (e1, env, (NotHole env):k)
eval1 (e1, env, (NotHole env2):k) | isValue e1 =  (bNOT e1, env2, k)
-- Rule No. 30
eval1 ((List str e1), env, k) =  (e1, env, k)
-- Rule No. 31
eval1 ((Hashes e1 e2), env, k) =  (e1, env, (HashesHole e2):k)
eval1 (e1, env, (HashesHole e2):k) =  (e2, env, k)
-- Rule No. 32
eval1 ((Hash e1), env, k) =  (e1, env, (HashHole):k)
eval1 (e1, env, (HashHole):k) =  (e1, env, k)
-- Rule No. 33
eval1 ((Print e1), env2, k) = ((Print e1), env2, k)

-- Function to iterate the small step reduction to termination
evalLoop :: Exp -> Environment -> String
evalLoop e env = evalLoop' (e,env,[])
      where evalLoop' :: Configuration -> String
            evalLoop' (ex1,env2,k) = case decideSemi (ex1,env2,k) of 
                                          (Print ex1, env2, k) -> printTile ex1 env2
                                          (ex1, env2, (SemisHole ex2):k) -> evalLoop' (ex2,env2,k)
            
            decideSemi :: Configuration -> Configuration
            decideSemi w@(Print ex1, env2, k) = w
            decideSemi (Semis ex1 ex2, env2, k) = semiEval (ex1, env2, (SemisHole ex2):k)
            
            semiEval :: Configuration -> Configuration
            semiEval cnf@(ex1, env3, (SemisHole ex2):k3) | isValue ex1 = cnf
            semiEval cnf = semiEval $ eval1 cnf

-- ^^^ CEK Machine ^^^ --

-- VVV For Testing Purposes Only VVV --
test :: Configuration -> Exp -> Bool
test c@(e1,env,k) result | e1 == result = True
                         | otherwise = test (testEv c) result

testEv x = eval1 x

testJoin = ((Assign "row1" (Join (List "row1" (List "tile1" (Single "tile2"))))),[("row1",Tile (Row (Blocks 1 (Block 0)))),("tile1",Tile (Row (Block 1))),("row2", Tile (Row (Blocks 0 (Block 1)))),("tile2",Tile (Row (Block 0)))],[])
testStack = ((Assign "outTile" (Stack (List "row1" (Single "row2")))),[("row1",Tile (Row (Blocks 1 (Block 0)))),("tile1",Tile (Row (Block 1))),("row2", Tile (Row (Blocks 0 (Block 1)))),("tile2",Tile (Row (Block 0)))],[])
testFor = ((For (Int 3) (Hash (Assign "outTile" (Stack (List "outTile" (List "row1" (Single "row2"))))))),[("outTile",testTile1),("row1",Tile (Row (Blocks 1 (Block 0)))),("row2", Tile (Row (Blocks 0 (Block 1))))],[])
testFor2 = ((For (Int 3) (Hash (Assign "i" (Add (Var "i") (Int 1))))),[("i",Int 0)],[])
testFor3 = ((For (Int 3) (Hashes ((Assign "j" (Add (Var "j") (Int 2)))) (Hash (Assign "i" (Add (Var "i") (Int 1)))))),[("i",Int 0),("j",Int 0)],[])
testJoin2 = ((Join $ List "t1" $ Single "t2"),[("t1", Tile $ Row $ Block 1), ("t2", Tile ((Rows (Block 1)) (Row $ Block 0)))],[])
testAdd = ((Add (Var "i") (Var "j")),[("i",Int 3),("j",Int 4)],[])
testMath = ((Mult (Add (Var "i") (Var "j")) (Add (Var "i") (Var "j"))),[("i",Int 3),("j",Int 4)],[])
testMath2 = ((Div (Add (Var "i") (Var "j")) (Minus (Var "i") (Var "j"))),[("i",Int 3),("j",Int 4)],[])
testIf = ((If (Less (Var "i") (Var "j")) (Add (Var "i") (Var "j")) (ElseIf (Equality (Var "i") (Int 3)) (Minus (Var "i") (Var "j")) (Else (Mult (Var "i") (Var "j"))))),[("i",Int 3),("j",Int 4)],[])
testIf2 = ((If (Less (Var "i") (Var "j")) (Add (Var "i") (Var "j")) (ElseIf (Equality (Var "i") (Int 3)) (Minus (Var "i") (Var "j")) (Else (Mult (Var "i") (Var "j"))))),[("i",Int 4),("j",Int 4)],[])
testIf3 = ((If (Less (Var "i") (Var "j")) (Add (Var "i") (Var "j")) (ElseIf (Equality (Var "i") (Int 3)) (Minus (Var "i") (Var "j")) (Else (Mult (Var "i") (Var "j"))))),[("i",Int 3),("j",Int 2)],[])
testAssign = ((Assign "k" (Int 3)),[("i",Int 3),("j",Int 4)],[])
--testTGroup = (Semi (TGroup "tilezzz" (Single "tile1")),[("tile1",Tile (Row (Block 1)))],[])
--testAccessor = (Semi (Accessor "tilezzz" 0),[("tilezzz",Single "tile1"),("tile1",Tile (Row (Block 1)))],[])
testPrint = (Print (Var "outTile"),[("outTile",testAnswer2)],[])

--testActions1 = [(Semi (TGroup "tilezzz" (Single "tile1"))),(Semi (Assign "outTile" (Accessor "tilezzz" 0))),(Print (Var "outTile"))]
testEnv1 = [("tile1",testTile1)]
testTile1 = (Tile (Rows (Blocks 1 (Block 0)) (Row (Blocks 0 (Block 1)))))

testAnswer2 = (Tile (Row (Blocks 1 (Blocks 0 (Blocks 1 (Block 0))))))