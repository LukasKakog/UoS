-- Challenge 6
-- Compare Innermost Reduction for Arithmetic and its Church Encoding
data ArithExpr = Add ArithExpr ArithExpr | Mul ArithExpr ArithExpr | Section ArithExpr  | SecApp ArithExpr ArithExpr | ArithNum Int deriving (Show,Eq,Read) 
data LamExpr = LamApp LamExpr LamExpr | LamAbs Int LamExpr | LamVar Int deriving (Show,Eq,Read)

--ARITHMETIC REDUCTION--
checkForArithNum :: ArithExpr -> Bool
checkForArithNum (ArithNum _) = True
checkForArithNum _ = False

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

innerArithRedn1 :: ArithExpr -> Maybe ArithExpr
innerArithRedn1 expr | checkForArithNum expr = Nothing
                     | otherwise = Just (arithRed expr)

countArithRed :: Maybe ArithExpr -> Int -> Int
countArithRed (Nothing) x = x-1 
countArithRed (Just expr) x = countArithRed (innerArithRedn1 expr) (x+1)

--LAMBDA REDUCTION--

freeVar :: Int -> LamExpr -> Bool
freeVar x (LamVar y) = x == y
freeVar x (LamAbs y e) | x == y = False
freeVar x (LamAbs y e) | x /= y = freeVar x e
freeVar x (LamApp e1 e2) = (freeVar x e1) || (freeVar x e2)

rename :: Int -> LamExpr -> Int
rename x e | freeVar (x+1) e = rename (x+1) e
           | otherwise = (x+1)

subst :: LamExpr -> Int -> LamExpr -> LamExpr
subst (LamVar x) y e | x == y = e
subst (LamVar x) y e | x /= y = LamVar x
subst (LamAbs x e1) y e | x /= y && not (freeVar x e) = LamAbs x (subst e1 y e)
subst (LamAbs x e1) y e | x /= y && (freeVar x e) = let z = (rename x e1) in subst (LamAbs z (subst e1 x (LamVar z))) y e
subst (LamAbs x e1) y e | x == y = LamAbs x e1
subst (LamApp e1 e2) y e = LamApp (subst e1 y e) (subst e2 y e) 

checkForNormalForm :: LamExpr -> Bool 
checkForNormalForm (LamApp (LamAbs x e1) e2) = False
checkForNormalForm (LamApp e1 e2) = (checkForNormalForm e1) && (checkForNormalForm e2) 
checkForNormalForm (LamAbs x e) = checkForNormalForm e
checkForNormalForm _ = True

getLamDepth :: LamExpr -> Int -> Int
getLamDepth (LamVar _) c = c
getLamDepth (LamAbs x e) c = getLamDepth e c
getLamDepth (LamApp (LamAbs x e1) e2) c | (getLamDepth e1 c) < (getLamDepth e2 c) = getLamDepth e2 (c+1)  
                                        | otherwise = getLamDepth e1 (c+1) 
getLamDepth (LamApp e1 e2) c | (getLamDepth e1 c) < (getLamDepth e2 c) = getLamDepth e2 c  
                             | otherwise = getLamDepth e1 c

lambdaRed :: LamExpr -> LamExpr
lambdaRed (LamVar x) = (LamVar x)
lambdaRed (LamAbs x e) | (getLamDepth e 0 == 0) = (LamAbs x e)
                       | otherwise = LamAbs x (lambdaRed e)
lambdaRed (LamApp e@(LamAbs x e1) e2) | (getLamDepth e1 0 == 0) && (getLamDepth e2 0 == 0) = subst e1 x e2
                                      | (getLamDepth e1 0) < (getLamDepth e2 0) = LamApp e (lambdaRed e2)
                                      | otherwise = LamApp (lambdaRed e) e2
lambdaRed (LamApp e1 e2) | (getLamDepth e1 0) < (getLamDepth e2 0) = LamApp e1 (lambdaRed e2)
                         | otherwise = LamApp (lambdaRed e1) e2

innerRedn1 :: LamExpr -> Maybe LamExpr
innerRedn1 expr | checkForNormalForm expr = Nothing
                | otherwise = Just (lambdaRed expr)

countLambdaRed :: Maybe LamExpr -> Int -> Int
countLambdaRed (Just expr) x | (getLamDepth expr 0 /= 0) = countLambdaRed (innerRedn1 expr) (x+1)
                             | otherwise = x

--FINDING NUMBER OF REDUCTIONS NEEDED--
compareArithLam :: ArithExpr -> (Int,Int)
compareArithLam expr = (countArithRed (Just expr) 0, countLambdaRed (Just (churchEnc1 expr)) 0)

--CHANGE NOTHING BELOW HERE (C5)--
getLambdaFromNumber1 :: Int -> LamExpr
getLambdaFromNumber1 x = LamAbs 0 (LamAbs 1 $ (iterate (\l -> LamApp (LamVar 0) l) (LamVar 1)) !! x)

plus1 :: LamExpr
plus1 = LamAbs 0 (LamAbs 1 (LamAbs 2 (LamAbs 3 (LamApp (LamApp (LamVar 0) (LamVar 2)) (LamApp (LamApp (LamVar 1)(LamVar 2))(LamVar 3))))))

mult1 :: LamExpr
mult1 = LamAbs 0 (LamAbs 1 (LamAbs 2 (LamAbs 3 (LamApp (LamApp (LamVar 0) (LamApp (LamVar 1) (LamVar 2))) (LamVar 3)))))

churchEnc1 :: ArithExpr -> LamExpr 
churchEnc1 (ArithNum x) = getLambdaFromNumber1 x
churchEnc1 (Add x y) = LamApp (LamApp (plus1) (churchEnc1 x)) (churchEnc1 y)
churchEnc1 (Mul x y) = LamApp (LamApp (mult1) (churchEnc1 x)) (churchEnc1 y)
churchEnc1 (SecApp (Section x) y) = LamApp (LamApp (plus1) (churchEnc1 x)) (churchEnc1 y)

--for testing--
test0 = (ArithNum 4)
test1 = (Add (ArithNum 4) (ArithNum 5))
test2 = (Mul (Add (ArithNum 4) (ArithNum 5)) (ArithNum 3))
test3 = (Mul (Add (SecApp (Section (ArithNum 1)) (ArithNum 4)) (ArithNum 5)) (ArithNum 3))
test4 = (Mul (Add (ArithNum 1) (ArithNum 2)) (Add (ArithNum 1) (Add (ArithNum 2) (ArithNum 3))))
test5 = Add (ArithNum 4) (Mul (ArithNum 5) (ArithNum 3))