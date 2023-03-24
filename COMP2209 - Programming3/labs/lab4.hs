
import Data.Char
import Data.List
import Data.Function

--ex1--

all1 :: (a -> Bool) -> [a] -> Bool
all1 p = foldr (&&) True . map p
            
any1 :: (a -> Bool) -> [a] -> Bool
any1 p = foldr (||) False . map p

takeWhile1 :: (a -> Bool) -> [a] -> [a]
takeWhile1 p = foldr (taking) []
    where taking x xs = if p x then x : xs else []

--ex2--

dec2int :: [Int] -> Int 
dec2int = foldl (\n m -> n * 10 + m ) 0

--ex3--

curry1 :: ((a,b) -> c) -> (a -> b -> c)
curry1 f = \a b -> f(a,b)

uncurry1 :: (a -> b -> c) -> ((a,b) -> c)
uncurry1 f = \(a,b) -> f a b

--ex4--

unfold :: (a -> Bool) -> (a -> b) -> (a -> a) -> a -> [b]
unfold p h t x | p x       = []
               | otherwise = h x : unfold p h t (t x)

int2bin :: Int -> [Int]
int2bin = unfold (== 0) (`mod` 2) (`div` 2)

chop :: Int -> String -> [String]
chop n = unfold (null) (take n) (drop n)

map2 :: (a -> b) -> [a] -> [b]
map2 f = unfold (null) (f.head) (tail) 

iterate2 :: (a -> a) -> a -> [a] 
iterate2 = unfold (\_ -> False) (id)

--ex5--

altMap :: (a -> b) -> (a -> b) -> [a] -> [b]
altMap f g [] = []
altMap f g (a:as) = f a : altMap g f as

--ex6--

luhnDouble :: Int -> Int
luhnDouble x | x > 4 = x * 2 - 9
             | otherwise = x * 2

luhn :: [Int] -> Bool 
luhn as = (sum $ altMap (id) (luhnDouble) (reverse as)) `mod` 10 == 0 

--ex7--

data Tree a = Leaf | Node (Tree a) a (Tree a) deriving Show

halve :: [a] -> ([a],a,[a])
halve xs = (take n xs , xs!!n , drop (n+1) xs) 
          where n = (length xs) `div` 2

balancing :: [a] -> Tree a
balancing [] = Leaf
balancing as = Node (balancing ls) a (balancing rs)
               where (ls,a,rs) = halve as

toTree :: Ord a => [a] -> Tree a
toTree = balancing . sort

--ex8--

data Nat = Zero | Succ Nat deriving (Eq,Ord,Show,Read)

evenNumber :: Nat -> Bool
evenNumber Zero = True
evenNumber (Succ x) = oddNumber x

oddNumber :: Nat -> Bool
oddNumber Zero = False
oddNumber (Succ x) = evenNumber x

addNumber :: Nat -> Nat -> Nat
addNumber Zero y = y
addNumber (Succ x) y = Succ (addNumber x y)

multiply :: Nat -> Nat -> Nat
multiply Zero y = Zero
multiply (Succ x) y = addNumber (multiply x y) y

--ex9--

data RInt = RZero | RSucc RInt | RPred RInt deriving Show

everythingPred :: RInt -> Bool
everythingPred RZero = True
everythingPred (RSucc n) = False
everythingPred (RPred n) = everythingPred n

everythingSucc :: RInt -> Bool
everythingSucc RZero = True
everythingSucc (RPred n) = False
everythingSucc (RSucc n) = everythingSucc n

removePred :: RInt -> RInt
removePred (RPred n) = n
removePred (RSucc n) = RSucc (removePred n)

removeSucc :: RInt -> RInt
removeSucc (RSucc n) = n
removeSucc (RPred n) = RPred (removeSucc n)

normalise :: RInt -> RInt
normalise RZero = RZero
normalise (RSucc n) | everythingSucc n = (RSucc n)
                    | otherwise = normalise (removePred n)
normalise (RPred n) | everythingPred n = (RPred n)
                    | otherwise = normalise (removeSucc n)

evenR :: RInt -> Bool 
evenR RZero = True 
evenR (RSucc x) = oddR x
evenR (RPred x) = oddR x

oddR :: RInt -> Bool  
oddR RZero = False
oddR (RSucc x) = evenR x
oddR (RPred x) = evenR x

addR :: RInt -> RInt -> RInt
addR RZero y = y
addR (RSucc x) y = normalise $ RSucc (addR x y)
addR (RPred x) y = normalise $ RPred (addR x y) 

negateR :: RInt -> RInt
negateR RZero = RZero
negateR (RSucc x) = RPred (negateR x) 
negateR (RPred x) = RSucc (negateR x) 

multR :: RInt -> RInt -> RInt
multR x y = multNorm (normalise x) (normalise y)
            where multNorm RZero y = RZero
                  multNorm (RSucc x) y = addR (multNorm x y) y
                  multNorm (RPred x) y = negateR (multNorm (negateR (RPred x)) y)

--A1--

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

--A2--

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

--A3--       

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
renderMaze ms = [ [doHash (elem (x,y) (allCoords ms)) | x <- [0.. (fst $ findLargest $ toList ms)]] | y <- [0.. (snd $ findLargest $ toList ms)]]

