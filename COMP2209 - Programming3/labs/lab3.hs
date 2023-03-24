
--ex1--

ex1 = sum( [ x^2 | x <- [1,3..99]] ++ [y^3 | y <- [2,4..100]])

--ex2--

grid :: Int -> Int -> [(Int,Int)]
grid x y = [(x,y) | x <- [0..x], y <- [0..y]]

square :: Int -> [(Int,Int)]
square y = [(x,y) |  x <- [0..y], y <- [0..y], x /= y]

--ex3--

replicatee :: Int -> a -> [a]
replicatee x a = [a | x <- [1..x]]

--ex4--

pyths :: Int -> [(Int,Int,Int)]
pyths l = [(x,y,z) | x <-[1..l], y <-[1..l], z <-[1..l], x^2 + y^2 == z^2]

--ex5--

factors :: Int -> [Int]
factors n = [x | x <- [1..n-1], n `mod` x == 0]

perfect :: Int -> [Int]
perfect l = [x | x <- [1..l], x == sum(factors x) ]

--ex6--

find :: Eq a => a -> [ (a,b)] -> [b]
find k t = [ v | (k',v) <- t, k==k']

positions :: Eq a => a -> [a] -> [Int]
positions e ls = find e [(x,y) | (x,y) <- zip ls [0..]] 

--ex7--

scalarproduct :: [Int] -> [Int] -> Int 
scalarproduct as bs = sum [x * y | (x,y) <- zip as bs]

--ex8--

shorter :: [a] -> [b] -> Bool
shorter as bs | size as < size bs = True
              | otherwise = False
                  where size [] = 0
                        size (x:xs) = 1 + size xs

--ex9--

euclid :: Int -> Int -> Int 
euclid x y | x == y = x
           | x < y = calc y x
           | otherwise = calc x y
               where calc d r | d `mod` r == 0 = r
                              | otherwise = calc r (d `mod` r) 

--ex10--

minHamming :: Eq a => [[a]] -> Int
minHamming ass = minimum [ distHamming xs ys | (xs,ys) <- pairing ass ]
                  where pairing [] = []
                        pairing (a:as) = [ (a,b) | b <- as] ++ pairing as
                        distHamming [] [] = 0
                        distHamming [] _ = error "Strings not the same length"
                        distHamming _ [] = error "Strings not the same length"
                        distHamming (x:xs) (y:ys) | x == y = distHamming xs ys
                                                  | otherwise = 1 + distHamming xs ys

--ex11--

merge :: Ord a => [a] -> [a] -> [a]
merge [] bs = bs
merge as [] = as
merge (a:as) (b:bs) | a < b = [a] ++ merge as (b:bs)
                    | a > b = [b] ++ merge (a:as) bs
                    | otherwise = [a,b] ++ merge as bs

halve :: [a] -> ([a],[a])
halve [] = ([],[])
halve as = (take n as, drop n as)
    where n = (length as) `div` 2

mergeSort :: Ord a => [a] -> [a]
mergeSort [] = []
mergeSort [a] = [a]
mergeSort as = merge (mergeSort as1) (mergeSort as2)
               where (as1, as2) = halve as
                     