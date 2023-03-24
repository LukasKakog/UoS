
-- Challenge 5
-- Church Encoding of arithmetic 
data ArithExpr = Add ArithExpr ArithExpr | Mul ArithExpr ArithExpr | Section ArithExpr  | SecApp ArithExpr ArithExpr | ArithNum Int deriving (Show,Eq,Read) 
data LamExpr = LamApp LamExpr LamExpr | LamAbs Int LamExpr | LamVar Int deriving (Show,Eq,Read)

getLambdaFromNumber :: Int -> LamExpr
getLambdaFromNumber x = LamAbs 0 (LamAbs 1 $ (iterate (\l -> LamApp (LamVar 0) l) (LamVar 1)) !! x)

plus :: LamExpr
plus = LamAbs 0 (LamAbs 1 (LamAbs 2 (LamAbs 3 (LamApp (LamApp (LamVar 0) (LamVar 2)) (LamApp (LamApp (LamVar 1)(LamVar 2))(LamVar 3))))))

mult :: LamExpr
mult = LamAbs 0 (LamAbs 1 (LamAbs 2 (LamAbs 3 (LamApp (LamApp (LamVar 0) (LamApp (LamVar 1) (LamVar 2))) (LamVar 3)))))

churchEnc :: ArithExpr -> LamExpr 
churchEnc (ArithNum x) = getLambdaFromNumber x
churchEnc (Add x y) = LamApp (LamApp (plus) (churchEnc x)) (churchEnc y)
churchEnc (Mul x y) = LamApp (LamApp (mult) (churchEnc x)) (churchEnc y)
churchEnc (SecApp (Section x) y) = LamApp (LamApp (plus) (churchEnc x)) (churchEnc y)

--testing--
test5 = Add (SecApp (Section (ArithNum 1)) (ArithNum 2)) (ArithNum 3)
