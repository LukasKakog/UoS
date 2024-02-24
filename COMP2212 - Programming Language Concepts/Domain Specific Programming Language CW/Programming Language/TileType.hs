module TileType where 
import Grammar 

type TypeEnvironment = [(String,Type)]

lookup2 :: String -> TypeEnvironment -> Type
lookup2 _ [] = error "no bindings"
lookup2 y ((x,z):xs) | x == y = z
                     | otherwise = lookup2 y xs                                 

bind :: (String,(Type, TypeEnvironment)) -> TypeEnvironment -> TypeEnvironment
bind (x,(t,e)) ys = ((x,t):ys)

unparseType :: Type -> String 
unparseType (IntType) = "Int"
unparseType (BoolType) = "Bool"
unparseType (TGType) = "Tile Group"
unparseType (TileType) = "Tile"

extract :: (Type,TypeEnvironment) -> Type
extract (t, _) = t

getHash :: Type -> Bool 
getHash (HashType _) = True 
getHash (HashList _ _) = True
getHash _ = False 

getSemi :: Type -> Bool 
getSemi (SemiList _ _) = True
getSemi _ = False 

typeOf :: TypeEnvironment -> Exp -> (Type,TypeEnvironment)
typeOf xs (Int _)  = (IntType, xs)
typeOf xs (BFalse) = (BoolType, xs)
typeOf xs (BTrue ) = (BoolType, xs)
typeOf xs (TGroup x e1) | extract (typeOf xs e1) == TGType = typeOf (bind (x,(typeOf xs e1)) xs) e1
                        | otherwise = error "Wrong type: TGroup accepts only list."
typeOf xs (List x e1) | ((lookup2 x xs) == TileType) && (extract (typeOf xs e1) == TileType) = (TGType, xs)
                      | otherwise = error "Wrong types: list only accepts tiles."
typeOf xs (Single x) | (lookup2 x xs) == TileType = (TileType, xs)
                     | otherwise = error "Wrong type: Single is a singular list of Tiles."

typeOf xs (Tile e1) | extract (typeOf xs e1) == RowType = (TileType, xs)
                    | otherwise = error "Wrong types: tiles on accepts a list of rows."
                    
typeOf xs (For e1 e2) | ((extract $ typeOf xs e1) /= IntType) = error "Wrong type: the for-loop does not have an int."
                      | not (getHash (extract $ typeOf xs e2)) = error "Wrong type: the for-loop has an invalid hash."
                      | otherwise = typeOf xs e2

typeOf xs (Var x) = (lookup2 x xs, xs)

typeOf xs (Div e1 e2) | (extract (typeOf xs e1) == IntType) &&  ( extract (typeOf xs e2) == IntType) = (IntType, xs)
                      | otherwise = error "Wrong types: '/' operator expected Ints."
typeOf xs (Minus e1 e2) | (extract (typeOf xs e1) == IntType) &&  (extract (typeOf xs e2) == IntType) = (IntType, xs)
                        | otherwise = error "Wrong types: '-' operator expected Ints."

typeOf xs (Mult e1 e2) | (extract (typeOf xs e1) == IntType) &&  (extract (typeOf xs e2) == IntType) = (IntType, xs)
                       | otherwise = error "Wrong types: '*' operator expected Ints."
typeOf xs (Add e1 e2)  | (extract (typeOf xs e1) == IntType) &&  (extract (typeOf xs e2) == IntType) = (IntType, xs)  
                       | otherwise = error "Wrong types: '+' operator expected Ints."        

typeOf xs (Less e1 e2)     | (extract (typeOf xs e1) == IntType) &&  (extract (typeOf xs e2) == IntType) = (BoolType, xs)
                           | otherwise = error "Wrong types: '<' operator expected Ints."                      

typeOf xs (Not e1)  | extract (typeOf xs e1) == BoolType = (BoolType, xs)
                    | otherwise = error "Wrong types: \"not\" operator expected Booleans."

typeOf xs (Or e1 e2) | (extract (typeOf xs e1) == BoolType) &&  (extract (typeOf xs e2) == BoolType) = (BoolType, xs)
                     | otherwise = error "Wrong types: \"||\" operator expected Booleans."

typeOf xs (And e1 e2) | (extract (typeOf xs e1) == BoolType) &&  (extract (typeOf xs e2) == BoolType) = (BoolType, xs)
                      | otherwise = error "Wrong types: \"&&\" operator expected Booleans."

typeOf xs (Equality e1 e2) | typee1 == typee2 = (BoolType, xs) 
                           | otherwise = error "Wrong types: \"==\" operator expected same types."
  where typee1 = extract $ typeOf xs e1
        typee2 = extract $ typeOf xs e2



typeOf xs (If e1 e2 e3) | (extract (typeOf xs e1) /= BoolType) = error "Wrong type: if-statement is not taking a Boolean."
                        | (extract (typeOf xs e2) /=  extract (typeOf xs e3)) = error "Wrong types: 'then' and 'else' clause are not of same type."
                        | otherwise = typeOf xs e2

typeOf xs (Else e1) = typeOf xs e1

typeOf xs (ElseIf e1 e2 e3) | (extract (typeOf xs e1) /= BoolType) = error "Wrong type: if-statement is not taking a Boolean."
                            | (extract (typeOf xs e2) /= extract (typeOf xs e3)) = error "Wrong types: 'then' and 'else' clause are not of same type."
                            | otherwise = typeOf xs e2
                            
typeOf xs (Block x)  =  if (x == 0 || x == 1) then (BlockType, xs) else error "Block only accepts int 1 and int 0."
typeOf xs (Blocks x e1) | (x == 0 || x == 1) && (extract (typeOf xs e1) == BlockType) = typeOf xs e1
                        | otherwise = error "Blocks must consist of 1 and 0 only."

typeOf xs (Rotate x) | extract (typeOf xs x) == TileType =  (FunctionType TileType TileType, xs) 
                     | otherwise = error "Wrong type: expected Tile in rotate."

typeOf xs (Negation x) | extract (typeOf xs x) == TileType =  (FunctionType TileType TileType, xs) 
                       | otherwise = error "Wrong type: expected Tile in negation."

typeOf xs (Conjunction e1 e2) | (extract (typeOf xs e1) == TileType) && (extract (typeOf xs e2) == TileType) =  ((FunctionType (PairType TileType TileType) TileType), xs) 
                              | otherwise = error "Wrong type: expected pair of tiles in conjunction."

typeOf xs (Row e1) | extract (typeOf xs e1) == BlockType = (RowType, xs)
                   | otherwise = error "Wrong type: rows only accept block types."

typeOf xs (Rows e1 e2) | (extract (typeOf xs e1) == BlockType) && (extract (typeOf xs e1) == RowType) = typeOf xs e2
                       | otherwise = error "Wrong type: rows only accepts block types."

typeOf xs (Blank e1 e2) | ((extract (typeOf xs e1)) == IntType) && ((extract (typeOf xs e1)) == IntType) =  (TileType, xs)
                        | otherwise = error " Wrong type: blank must take expressions of type Int."

typeOf xs (NumCol e1) | extract (typeOf xs e1) == TileType = (IntType, xs)
                      | otherwise = error "Wrong type: numCol expects a tile."

typeOf xs (NumRow e1) | extract (typeOf xs e1) == TileType = (IntType, xs)
                      | otherwise = error "Wrong type: numRow expects a tile."


typeOf xs (Print e1) | extract (typeOf xs e1) == TileType = (TileType, xs)
                     | otherwise = error "Wrong type: print only accepts tiles."


typeOf xs (FlipX x)    | extract (typeOf xs x) == TileType =  (FunctionType TileType TileType, xs) 
                       | otherwise = error "Wrong type: flipx accepts only tiles."

typeOf xs (FlipY x)    | extract (typeOf xs x) == TileType =  (FunctionType TileType TileType, xs) 
                       | otherwise = error "Wrong type: flipy accepts only tiles."

typeOf xs (Join x)     | extract (typeOf xs x) == TGType =  (FunctionType TGType TileType, xs) 
                       | otherwise = error "Wrong type: join accepts only a group of tiles."

typeOf xs (Stack x)    | extract (typeOf xs x) == TGType =  (FunctionType TGType TileType, xs) 
                       | otherwise = error "Wrong type: stack accepts only  a group of tiles."

typeOf xs (Sub e1 e2 e3 e4) | (extract (typeOf xs e1) /= TileType) = error "Wrong type: the first argument in subtile is not a tile."
                            | not $ isInt xs e2 = error "Wrong type: the second argument in subtile is not of type Int." 
                            | not $ isInt xs e3 = error "Wrong type: the third argument in subtile is not of type Int." 
                            | not $ isInt xs e4 = error "Wrong type: the fourth argument in subtile is not of type Int." 
                            | otherwise = (FunctionType TileType TileType, xs)

typeOf xs (Super e1 e2 ) | (extract (typeOf xs e1) == TileType) && (extract (typeOf xs e2) == IntType) = (FunctionType TileType TileType, xs)
                        | otherwise = error "Wrong type: supersize only accepts tiles."                            

typeOf xs (Assign x e1) = typeOf (bind (x,(typeOf xs e1)) xs) e1

typeOf xs (Accessor e1 x) | (lookup2 e1 xs) /= TGType = error "Wrong type: accessor only accepts TGroup."
                          | isInt xs x = error "Wrong type: accessor's second argument is not an Int."
                          | otherwise = (TileType, xs)

typeOf xs (Semis e1 e2) | getSemi (extract (typeOf xs e2)) = (SemiList (extract (typeOf xs e1)) (extract (typeOf xs e2)),xs)

typeOf xs (Hash e1) = (HashType (extract (typeOf xs e1)),xs)
typeOf xs (Hashes e1 e2) | getHash (extract (typeOf xs e2)) = (HashList (extract (typeOf xs e1)) (extract (typeOf xs e2)),xs)  

--Checks if an Exp is in Int
isInt :: TypeEnvironment -> Exp -> Bool
isInt xs x | extract (typeOf xs x) == IntType = True
           | otherwise = False