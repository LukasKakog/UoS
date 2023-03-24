
--ex2

sum [] = 0
sum (x:xs) = x + sum xs

--ex3

product [] = 1
product (x:xs) = x * product xs

--ex7

n = a `div` length xs
	    where 
	       a = 10 
	       xs = [1,2,3,4,5]