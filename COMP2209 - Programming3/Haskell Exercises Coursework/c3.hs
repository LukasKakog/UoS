import Data.List

-- Challenge 3
-- Pretty Printing Lambda with Alpha-Normalisation 

data LamExpr = LamApp LamExpr LamExpr | LamAbs Int LamExpr | LamVar Int deriving (Show,Eq,Read)

freeVar :: Int -> LamExpr -> Bool
freeVar x (LamVar y) = x == y
freeVar x (LamAbs y e) | x == y = False
freeVar x (LamAbs y e) | x /= y = freeVar x e
freeVar x (LamApp e1 e2) = (freeVar x e1) || (freeVar x e2)

substitution :: LamExpr -> Int -> Int -> LamExpr
substitution (LamVar x) l z | x == l = LamVar z
substitution (LamVar x) l z | x /= l = LamVar x
substitution (LamAbs x y) l z | x /= l && (x /= z) = LamAbs x (substitution y l z)
substitution (LamAbs x y) l z | x /= l && (x == z) = let s = (rename x y) in substitution (LamAbs s (substitution y x s)) l z
substitution (LamAbs x y) l z | x == l = LamAbs x y
substitution (LamApp x y) l z = LamApp (substitution x l z) (substitution y l z) 

rename :: Int -> LamExpr -> Int
rename x e | freeVar (x+1) e = rename (x+1) e
           | otherwise = (x+1)

alphaConversion :: LamExpr -> LamExpr
alphaConversion (LamVar x) = LamVar x 
alphaConversion (LamAbs x y) = LamAbs new $ alphaConversion $ substitution y x new
                                  where new = head [v | v <- [0..], not $ freeVar v (LamAbs x y)]  
alphaConversion (LamApp x y) = LamApp (alphaConversion x) (alphaConversion y)

toString :: LamExpr -> String
toString (LamVar x) = "x" ++ show x
toString (LamAbs x y) = "\\x" ++ (show x) ++ " -> " ++ toString y 
toString (LamApp (LamAbs x y) z) = "(" ++ "\\x" ++ (show x) ++ " -> " ++ toString y ++ ") " ++ toString z
toString (LamApp x y) = toString x ++ " " ++ toString y

prettyPrint :: LamExpr -> String
prettyPrint x = toString $ alphaConversion x 

--for testing--
test1 = (LamApp (LamVar 1) (LamVar 0))                                                      -- "x1 x0"
test2 = (LamAbs 3 (LamVar 2))                                                               -- "\\x0 -> x2"
test3 = (LamAbs 0 (LamAbs 1 (LamVar 0)))                                                    -- "\\x0 -> \\x1 -> x0"
test4 = (LamAbs 1 (LamAbs 0 (LamVar 1)))                                                    -- "\\x0 -> \\x1 -> x0"
test5 = (LamAbs 1 (LamAbs 0 (LamVar 0)))                                                    -- "\\x0 -> \\x0 -> x0"
test6 = (LamAbs 0 (LamAbs 1 (LamAbs 2 (LamVar 0))))                                         -- "\\x0 -> \\x1 -> \\x1 -> x0"
test7 = (LamApp (LamAbs 1 (LamVar 1)) (LamAbs 1 (LamVar 1)))                                -- "(\\x0 -> x0) \\x0 -> x0"
test8 = (LamAbs 1 (LamApp (LamVar 1) (LamAbs 1 (LamVar 1))))                                -- "\\x0 -> x0 \\x0 -> x0"
test9 = (LamApp (LamVar 2) (LamAbs 1 (LamAbs 2 (LamVar 1))))                                -- "x2 \\x0 -> \\x1 -> x0"
test10 = (LamAbs 1 (LamAbs 1 (LamApp (LamVar 1) (LamAbs 1 (LamAbs 2 (LamVar 1))))))         -- "\\x0 -> \\x0 -> x0 \\x0 -> \\x1 -> x0"
test11 = (LamAbs 1 (LamAbs 2 (LamApp (LamAbs 3 (LamVar 1)) (LamVar 1))))                    -- "\\x0 -> \\x1 -> (\\x1 -> x0) x0"
test12 = (LamAbs 1 (LamApp (LamVar 1) (LamAbs 0 (LamVar 0))))                               -- "\\x0 -> x0 \\x1 -> x1"
test13 = (LamApp (LamAbs 1 (LamVar 1)) (LamAbs 2 (LamVar 2)))                               -- "(\\x0 -> x0) \\x0 -> x0"
test14 = (LamAbs 3 (LamAbs 2 (LamAbs 2 (LamVar 3))))                                        -- "\\x0 -> \\x1 -> \\x1 -> x0"
