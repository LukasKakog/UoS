import Data.Char
import Parsing

-- Challenge 4 
-- Parsing Arithmetic Expressions
data ArithExpr = Add ArithExpr ArithExpr | Mul ArithExpr ArithExpr | Section ArithExpr  | SecApp ArithExpr ArithExpr | ArithNum Int deriving (Show,Eq,Read) 

addition :: Parser ArithExpr
addition = do e1 <- multiplication
              char '+'
              e2 <- addition
              return (Add e1 e2)
           <|> multiplication

multiplication :: Parser ArithExpr
multiplication = do e1 <- secApp
                    char '*'
                    e2 <- multiplication
                    return (Mul e1 e2)
                 <|> secApp

secApp :: Parser ArithExpr
secApp = do char '('
            char '+'
            e1 <- addition
            char ')'
            e2 <- natExpr <|> addition
            return (SecApp (Section e1) e2)
         <|> sec
      
sec :: Parser ArithExpr
sec = natExpr
      <|> do char '('
             e1 <- addition
             char ')'
             return (e1)

natExpr :: Parser ArithExpr
natExpr = do xs <- some digit
             return (ArithNum (read xs))

filterSpaces :: String -> String
filterSpaces xs = filter (\x -> x /= ' ') xs

parseArith :: String -> Maybe ArithExpr
parseArith xs | elem '-' xs = Nothing
              | (snd (head (parse addition (filterSpaces xs)))) /= [] = Nothing
              | (parse addition (filterSpaces xs)) == [] = Nothing
              | otherwise = Just (fst (head (parse addition (filterSpaces xs))))
