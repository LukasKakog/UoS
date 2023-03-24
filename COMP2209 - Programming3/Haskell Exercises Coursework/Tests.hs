import Challenges

--function for testing specific challenges at once
testChal :: (Int -> Bool) -> [Int] -> [Bool]
testChal f xs = map f xs

--Challenge 1 tests
c1 :: Int -> Bool
c1 1 = ((calcBBInteractions 1 [] [(South,2)]) == [((South,2),Path (North,2))]) --test for no atoms
c1 2 = ((calcBBInteractions 3 [(2,2)] [(West,1)]) == [((West,1),Path (North,1))]) --test for 1 atom
c1 3 = ((calcBBInteractions 4 [(2,2),(3,2)] [(West,2)]) == [((West,2), Absorb)]) --test for absorption precedence
c1 4 = ((calcBBInteractions 4 [(2,1),(3,1)] [(North,3)]) == [((North,3), Absorb)]) --test for absorption precedence on a border
--testing for larger sizes and more atoms
c1 5 = ((calcBBInteractions 8 [(2,3),(7,3),(4,6),(7,8)] [(North,1),(North,5),(East,7),(South,7),(West,3),(West,1)]) == [((North,1),Path (West,2)),((North,5),Path (East,5)),((East,7),Path (East,4)),((South,7),Absorb),((West,3),Absorb),((West,1),Path (East,1))])
c1 6 = ((calcBBInteractions 12 [(2,2),(5,2),(4,9),(1,9),(12,11)] [(North,1),(North,10),(East,10),(South,4),(West,6),(East,3)]) == [((North,1),Path (West,1)),((North,10),Path (South,10)),((East,10),Reflect),((South,4),Absorb),((West,6),Path (East,6)),((East,3),Path (South,6))])
c1 7 = ((calcBBInteractions 25 [(2,2),(5,2),(4,9),(1,9),(8,14),(21,3),(22,13)] [(South,4),(North,6),(East,8),(South,21),(West,13),(East,1),(North,4)]) == [((South,4),Absorb),((North,6),Path (East,1)),((East,8),Absorb),((South,21),Absorb),((West,13),Path (North,7)),((East,1),Path (North,6)),((North,4),Path (North,3))])

--Challenge 2 tests
c2 :: Int -> Bool
c2 1 = ((solveBB 1 [((North,2), Path (South,2))]) == []) --test for no atoms
c2 2 = ((solveBB 3 [((North,1), Path (West,1))]) == [(2,2)]) --test for 1 atom
c2 3 = ((solveBB 3 [((North,3), Absorb), ((South,2), Absorb), ((West,2), Absorb)]) == [(2,2),(3,2)])
--test for larger sizes and atoms
c2 4 = ((solveBB 5 [((South,1), Path (West,4))]) == [(2,3)])
c2 5 = (length (solveBB 5 [((South,4), Absorb)]) == 1)
c2 6 = ((solveBB 8 [((North,1),Path (West,2)), ((North,5),Path (East,5)),((East,7),Path (East,4)),((South,7),Absorb), ((West,1),Path (East,1)), ((West,3),Absorb)]) == [(2,3),(7,3),(4,6),(7,8)])
c2 7 = ((solveBB 10 [((North,1), Path (West,1)),((East,3), Absorb),((West,4), Absorb),((North,5),Absorb),((South,5), Absorb)] == [(2,2),(5,3),(5,4)]))

--Challenge 3 tests
c3 :: Int -> Bool
c3 1 = (prettyPrint (LamApp (LamVar 1) (LamVar 0)) == "x1 x0" )
c3 2 = (prettyPrint (LamAbs 3 (LamVar 2)) == "\\x0 -> x2")
c3 3 = (prettyPrint (LamAbs 0 (LamAbs 1 (LamVar 0))) == "\\x0 -> \\x1 -> x0")
c3 4 = (prettyPrint (LamAbs 1 (LamAbs 0 (LamVar 1))) == "\\x0 -> \\x1 -> x0")                         
c3 5 = (prettyPrint (LamAbs 1 (LamAbs 0 (LamVar 0))) == "\\x0 -> \\x0 -> x0")                                     
c3 6 = (prettyPrint (LamAbs 0 (LamAbs 1 (LamAbs 2 (LamVar 0)))) == "\\x0 -> \\x1 -> \\x1 -> x0")                   
c3 7 = (prettyPrint (LamApp (LamAbs 1 (LamVar 1)) (LamAbs 1 (LamVar 1))) == "(\\x0 -> x0) \\x0 -> x0")                     
c3 8 = (prettyPrint (LamAbs 1 (LamApp (LamVar 1) (LamAbs 1 (LamVar 1)))) == "\\x0 -> x0 \\x0 -> x0")                      
c3 9 = (prettyPrint (LamApp (LamVar 2) (LamAbs 1 (LamAbs 2 (LamVar 1)))) == "x2 \\x0 -> \\x1 -> x0")                         
c3 10 = (prettyPrint (LamAbs 1 (LamAbs 1 (LamApp (LamVar 1) (LamAbs 1 (LamAbs 2 (LamVar 1)))))) == "\\x0 -> \\x0 -> x0 \\x0 -> \\x1 -> x0")        
c3 11 = (prettyPrint (LamAbs 1 (LamAbs 2 (LamApp (LamAbs 3 (LamVar 1)) (LamVar 1)))) == "\\x0 -> \\x1 -> (\\x1 -> x0) x0")            
c3 12 = (prettyPrint (LamAbs 1 (LamApp (LamVar 1) (LamAbs 0 (LamVar 0)))) == "\\x0 -> x0 \\x0 -> x0")                         
c3 13 = (prettyPrint (LamApp (LamAbs 1 (LamVar 1)) (LamAbs 2 (LamVar 2))) == "(\\x0 -> x0) \\x0 -> x0")                             
c3 14 = (prettyPrint (LamAbs 3 (LamAbs 2 (LamAbs 2 (LamVar 3)))) == "\\x0 -> \\x1 -> \\x1 -> x0")  

--Challenge 4 tests
c4 :: Int -> Bool
c4 1 = (parseArith ("1 + 2") == Just (Add (ArithNum 1) (ArithNum 2)))
c4 2 = (parseArith ("1                 + 3") == Just (Add (ArithNum 1) (ArithNum 3)))
c4 3 = (parseArith ("1*2+4") == Just (Add (Mul (ArithNum 1) (ArithNum 2)) (ArithNum 4)))
c4 4 = (parseArith ("1 (+2)") == Nothing)
c4 5 = (parseArith ("1 + (+2)3") == Just (Add (ArithNum 1) (SecApp (Section (ArithNum 2)) (ArithNum 3))))
c4 6 = (parseArith ("1 2") == Just (ArithNum 12))
c4 7 = (parseArith ("(+1) 2 + 4") == Just (Add (SecApp (Section (ArithNum 1)) (ArithNum 2)) (ArithNum 4)))

--Challenge 5 tests
c5 :: Int -> Bool
c5 1 = (churchEnc (ArithNum 4) == LamAbs 0 (LamAbs 1 (LamApp (LamVar 0) (LamApp (LamVar 0) (LamApp (LamVar 0) (LamApp (LamVar 0) (LamVar 1)))))))
c5 2 = (churchEnc (Add (ArithNum 4) (ArithNum 5)) == LamApp (LamApp (LamAbs 0 (LamAbs 1 (LamAbs 2 (LamAbs 3 (LamApp (LamApp (LamVar 0) (LamVar 2)) (LamApp (LamApp (LamVar 1) (LamVar 2)) (LamVar 3))))))) (LamAbs 0 (LamAbs 1 (LamApp (LamVar 0) (LamApp (LamVar 0) (LamApp (LamVar 0) (LamApp (LamVar 0) (LamVar 1)))))))) (LamAbs 0 (LamAbs 1 (LamApp (LamVar 0) (LamApp (LamVar 0) (LamApp (LamVar 0) (LamApp (LamVar 0) (LamApp (LamVar 0) (LamVar 1)))))))))
c5 3 = (churchEnc (Add (ArithNum 4) (Mul (ArithNum 5) (ArithNum 3))) == LamApp (LamApp (LamAbs 0 (LamAbs 1 (LamAbs 2 (LamAbs 3 (LamApp (LamApp (LamVar 0) (LamVar 2)) (LamApp (LamApp (LamVar 1) (LamVar 2)) (LamVar 3))))))) (LamAbs 0 (LamAbs 1 (LamApp (LamVar 0) (LamApp (LamVar 0) (LamApp (LamVar 0) (LamApp (LamVar 0) (LamVar 1)))))))) (LamApp (LamApp (LamAbs 0 (LamAbs 1 (LamAbs 2 (LamAbs 3 (LamApp (LamApp (LamVar 0) (LamApp (LamVar 1) (LamVar 2))) (LamVar 3)))))) (LamAbs 0 (LamAbs 1 (LamApp (LamVar 0) (LamApp (LamVar 0) (LamApp (LamVar 0) (LamApp (LamVar 0) (LamApp (LamVar 0) (LamVar 1))))))))) (LamAbs 0 (LamAbs 1 (LamApp (LamVar 0) (LamApp (LamVar 0) (LamApp (LamVar 0) (LamVar 1))))))))
c5 4 = (churchEnc (Mul (Add (ArithNum 4) (ArithNum 5)) (ArithNum 3)) == LamApp (LamApp (LamAbs 0 (LamAbs 1 (LamAbs 2 (LamAbs 3 (LamApp (LamApp (LamVar 0) (LamApp (LamVar 1) (LamVar 2))) (LamVar 3)))))) (LamApp (LamApp (LamAbs 0 (LamAbs 1 (LamAbs 2 (LamAbs 3 (LamApp (LamApp (LamVar 0) (LamVar 2)) (LamApp (LamApp (LamVar 1) (LamVar 2)) (LamVar 3))))))) (LamAbs 0 (LamAbs 1 (LamApp (LamVar 0) (LamApp (LamVar 0) (LamApp (LamVar 0) (LamApp (LamVar 0) (LamVar 1)))))))) (LamAbs 0 (LamAbs 1 (LamApp (LamVar 0) (LamApp (LamVar 0) (LamApp (LamVar 0) (LamApp (LamVar 0) (LamApp (LamVar 0) (LamVar 1)))))))))) (LamAbs 0 (LamAbs 1 (LamApp (LamVar 0) (LamApp (LamVar 0) (LamApp (LamVar 0) (LamVar 1)))))))
c5 5 = (churchEnc (Add (SecApp (Section (ArithNum 1)) (ArithNum 2)) (ArithNum 3)) == LamApp (LamApp (LamAbs 0 (LamAbs 1 (LamAbs 2 (LamAbs 3 (LamApp (LamApp (LamVar 0) (LamVar 2)) (LamApp (LamApp (LamVar 1) (LamVar 2)) (LamVar 3))))))) (LamApp (LamApp (LamAbs 0 (LamAbs 1 (LamAbs 2 (LamAbs 3 (LamApp (LamApp (LamVar 0) (LamVar 2)) (LamApp (LamApp (LamVar 1) (LamVar 2)) (LamVar 3))))))) (LamAbs 0 (LamAbs 1 (LamApp (LamVar 0) (LamVar 1))))) (LamAbs 0 (LamAbs 1 (LamApp (LamVar 0) (LamApp (LamVar 0) (LamVar 1))))))) (LamAbs 0 (LamAbs 1 (LamApp (LamVar 0) (LamApp (LamVar 0) (LamApp (LamVar 0) (LamVar 1)))))))
c5 6 = (churchEnc (Mul (Add (SecApp (Section (ArithNum 1)) (ArithNum 4)) (ArithNum 5)) (ArithNum 3)) == LamApp (LamApp (LamAbs 0 (LamAbs 1 (LamAbs 2 (LamAbs 3 (LamApp (LamApp (LamVar 0) (LamApp (LamVar 1) (LamVar 2))) (LamVar 3)))))) (LamApp (LamApp (LamAbs 0 (LamAbs 1 (LamAbs 2 (LamAbs 3 (LamApp (LamApp (LamVar 0) (LamVar 2)) (LamApp (LamApp (LamVar 1) (LamVar 2)) (LamVar 3))))))) (LamApp (LamApp (LamAbs 0 (LamAbs 1 (LamAbs 2 (LamAbs 3 (LamApp (LamApp (LamVar 0) (LamVar 2)) (LamApp (LamApp (LamVar 1) (LamVar 2)) (LamVar 3))))))) (LamAbs 0 (LamAbs 1 (LamApp (LamVar 0) (LamVar 1))))) (LamAbs 0 (LamAbs 1 (LamApp (LamVar 0) (LamApp (LamVar 0) (LamApp (LamVar 0) (LamApp (LamVar 0) (LamVar 1))))))))) (LamAbs 0 (LamAbs 1 (LamApp (LamVar 0) (LamApp (LamVar 0) (LamApp (LamVar 0) (LamApp (LamVar 0) (LamApp (LamVar 0) (LamVar 1)))))))))) (LamAbs 0 (LamAbs 1 (LamApp (LamVar 0) (LamApp (LamVar 0) (LamApp (LamVar 0) (LamVar 1)))))))
c5 7 = (churchEnc (Mul (Add (ArithNum 1) (ArithNum 2)) (Add (ArithNum 1) (Add (ArithNum 2) (ArithNum 3)))) == LamApp (LamApp (LamAbs 0 (LamAbs 1 (LamAbs 2 (LamAbs 3 (LamApp (LamApp (LamVar 0) (LamApp (LamVar 1) (LamVar 2))) (LamVar 3)))))) (LamApp (LamApp (LamAbs 0 (LamAbs 1 (LamAbs 2 (LamAbs 3 (LamApp (LamApp (LamVar 0) (LamVar 2)) (LamApp (LamApp (LamVar 1) (LamVar 2)) (LamVar 3))))))) (LamAbs 0 (LamAbs 1 (LamApp (LamVar 0) (LamVar 1))))) (LamAbs 0 (LamAbs 1 (LamApp (LamVar 0) (LamApp (LamVar 0) (LamVar 1))))))) (LamApp (LamApp (LamAbs 0 (LamAbs 1 (LamAbs 2 (LamAbs 3 (LamApp (LamApp (LamVar 0) (LamVar 2)) (LamApp (LamApp (LamVar 1) (LamVar 2)) (LamVar 3))))))) (LamAbs 0 (LamAbs 1 (LamApp (LamVar 0) (LamVar 1))))) (LamApp (LamApp (LamAbs 0 (LamAbs 1 (LamAbs 2 (LamAbs 3 (LamApp (LamApp (LamVar 0) (LamVar 2)) (LamApp (LamApp (LamVar 1) (LamVar 2)) (LamVar 3))))))) (LamAbs 0 (LamAbs 1 (LamApp (LamVar 0) (LamApp (LamVar 0) (LamVar 1)))))) (LamAbs 0 (LamAbs 1 (LamApp (LamVar 0) (LamApp (LamVar 0) (LamApp (LamVar 0) (LamVar 1)))))))))

--Challenge 6 tests
c6 :: Int -> Bool
c6 1 = (compareArithLam (ArithNum 4) == (0,0))
c6 2 = (compareArithLam (Add (ArithNum 4) (ArithNum 5)) == (1,6))
c6 3 = (compareArithLam (Add (ArithNum 4) (Mul (ArithNum 5) (ArithNum 3))) == (2,20))
c6 4 = (compareArithLam (Mul (Add (ArithNum 4) (ArithNum 5)) (ArithNum 3)) == (2,28))
c6 5 = (compareArithLam (Add (SecApp (Section (ArithNum 1)) (ArithNum 2)) (ArithNum 3)) == (2,12))
c6 6 = (compareArithLam (Mul (Add (SecApp (Section (ArithNum 1)) (ArithNum 4)) (ArithNum 5)) (ArithNum 3)) == (3,36))
c6 7 = (compareArithLam (Mul (Add (ArithNum 1) (ArithNum 2)) (Add (ArithNum 1) (Add (ArithNum 2) (ArithNum 3)))) == (4,28))
