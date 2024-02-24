{
 module Tokens where
}

%wrapper "posn"

$digit = 0-9            -- digits
$alpha = [a-zA-Z]        -- alphabetic characters

tokens :-

  $white+                ;
  "--".*                 ;
  $digit+                { tok (\p s -> TokenDigit p (read s) )}
  \-                     { tok (\p s -> TokenMinus p        )}
  \;                     { tok (\p s -> TokenSemiColon p    )}
  \#                     { tok (\p s -> TokenHash p         )}
  \*                     { tok (\p s -> TokenMult p         )}
  \/                     { tok (\p s -> TokenDiv p          )}
  \(                     { tok (\p s -> TokenOpBracket p    )}
  \)                     { tok (\p s -> TokenClBracket p    )}
  \=                     { tok (\p s -> TokenEq p           )}
  \+                     { tok (\p s -> TokenPlus p         )}
  \<                     { tok (\p s -> TokenLess p         )}
  \=\=                   { tok (\p s -> TokenEquality p     )}
  \,                     { tok (\p s -> TokenComma p        )}
  \[                     { tok (\p s -> TokenOSBracket p    )}
  \]                     { tok (\p s -> TokenClSBracket p   )}
  \{                     { tok (\p s -> TokenOCurBracket p  )}
  \}                     { tok (\p s -> TokenClCurBracket p )}
  import                 { tok (\p s -> TokenImport p       )}
  print                  { tok (\p s -> TokenPrint p        )}
  if                     { tok (\p s -> TokenIf p           )}
  then                   { tok (\p s -> TokenThen p         )}
  else                   { tok (\p s -> TokenElse p         )}
  for                    { tok (\p s -> TokenFor p          )}
  create                 { tok (\p s -> TokenCreate p       )}
  blank                  { tok (\p s -> TokenBlank p        )}
  flipx                  { tok (\p s -> TokenFlipX p        )}
  flipy                  { tok (\p s -> TokenFlipY p        )}
  print                  { tok (\p s -> TokenPrint p        )}
  rotate                 { tok (\p s -> TokenRotate p       )}
  subtile                { tok (\p s -> TokenSubtile p      )}
  conj                   { tok (\p s -> TokenConj p         )}
  neg                    { tok (\p s -> TokenNeg p          )}
  supersize              { tok (\p s -> TokenSupersize p    )}
  stack                  { tok (\p s -> TokenStack p        )}
  join                   { tok (\p s -> TokenJoin p         )}
  numCol                 { tok (\p s -> TokenNCol p         )}
  numRow                 { tok (\p s -> TokenNRow p         )}
  \&\&                   { tok (\p s -> TokenAND p          )}
  \|\|                   { tok (\p s -> TokenOR p           )}
  not                    { tok (\p s -> TokenNOT p          )}
  Int                    { tok (\p s -> TokenInt p          )}
  Tile                   { tok (\p s -> TokenTile p         )}
  TileGroup              { tok (\p s -> TokenTileG p        )}
  Bool                   { tok (\p s -> TokenBool p         )}
  true                   { tok (\p s -> TokenBTrue p        )}
  false                  { tok (\p s -> TokenBFalse p       )}
  \[ $alpha [$alpha $digit \_ \']*  \] {tok (\p s -> TokenGroup p s )}
  $alpha [$alpha $digit \_ \' \.]*  { tok (\p s -> TokenVar p s)}

{
-- Each right-hand side has type :: AlexPosn -> String -> Token
-- Some action helpers:
tok f p s = f p s

-- The token type:
data TokenType =
    TokenDigit AlexPosn Int          |
    TokenEquality AlexPosn           |
    TokenSemiColon AlexPosn          |
    TokenHash AlexPosn               |
    TokenPrint AlexPosn              |
    TokenMinus AlexPosn              |
    TokenMult AlexPosn               |
    TokenDiv  AlexPosn               |
    TokenOpBracket AlexPosn          |
    TokenClBracket AlexPosn          |
    TokenEq AlexPosn                 |
    TokenPlus AlexPosn               |
    TokenDot AlexPosn                |
    TokenLess AlexPosn               |
    TokenComma AlexPosn              |
    TokenOSBracket AlexPosn          |
    TokenClSBracket AlexPosn         |
    TokenClCurBracket AlexPosn       |
    TokenOCurBracket AlexPosn        |
    TokenIf AlexPosn                 |
    TokenThen AlexPosn               |
    TokenElse AlexPosn               |
    TokenFor AlexPosn                |
    TokenImport AlexPosn             |
    TokenCreate AlexPosn             |
    TokenBlank AlexPosn              |
    TokenFlipX AlexPosn              |
    TokenFlipY AlexPosn              |
    TokenRotate AlexPosn             |
    TokenSubtile AlexPosn            |
    TokenConj AlexPosn               |
    TokenNeg AlexPosn                |
    TokenSupersize AlexPosn          |
    TokenStack AlexPosn              |
    TokenJoin AlexPosn               |
    TokenShape AlexPosn              |
    TokenAND AlexPosn                |
    TokenOR AlexPosn                 |
    TokenNOT AlexPosn                |
    TokenInt AlexPosn                |
    TokenTile AlexPosn               |
    TokenTileG AlexPosn              |
    TokenBool AlexPosn               |
    TokenBTrue AlexPosn              |
    TokenBFalse AlexPosn             |
    TokenNCol AlexPosn               |
    TokenNRow AlexPosn               |
    TokenGroup AlexPosn String       |
    TokenVar AlexPosn String

    deriving (Eq,Show)

token_posn (TokenDigit (AlexPn a l c) d)  = show(l) ++ ":" ++ show(c) ++ " -> " ++ "digit " ++ show d
token_posn (TokenMinus (AlexPn a l c) ) = show(l) ++ ":" ++ show(c) ++ " -> " ++ "-"
token_posn (TokenMult (AlexPn a l c) ) = show(l) ++ ":" ++ show(c) ++ " -> " ++ "*"
token_posn (TokenDiv (AlexPn a l c) ) =  show(l) ++ ":" ++ show(c) ++ " -> " ++ "/"
token_posn (TokenOpBracket (AlexPn a l c) ) = show(l) ++ ":" ++ show(c) ++ " -> " ++ "("
token_posn (TokenClBracket (AlexPn a l c) ) =  show(l) ++ ":" ++ show(c) ++ " -> " ++ ")"
token_posn (TokenEq (AlexPn a l c) ) = show(l) ++ ":" ++ show(c) ++ " -> " ++ "="
token_posn (TokenEquality (AlexPn a l c) ) = show(l) ++ ":" ++ show(c) ++ " -> " ++ "=="
token_posn (TokenPlus (AlexPn a l c) ) = show(l) ++ ":" ++ show(c) ++ " -> " ++ "+"
token_posn (TokenDot (AlexPn a l c) ) = show(l) ++ ":" ++ show(c) ++ " -> " ++ "."
token_posn (TokenLess (AlexPn a l c) ) = show(l) ++ ":" ++ show(c) ++ " -> " ++ "<"
token_posn (TokenComma (AlexPn a l c) ) = show(l) ++ ":" ++ show(c) ++ " -> " ++ ","
token_posn (TokenOSBracket (AlexPn a l c) ) = show(l) ++ ":" ++ show(c) ++ " -> " ++ "["
token_posn (TokenClSBracket (AlexPn a l c) ) = show(l) ++ ":" ++ show(c) ++ " -> " ++ "]"
token_posn (TokenClCurBracket (AlexPn a l c) ) = show(l) ++ ":" ++ show(c) ++ " -> " ++ "{"
token_posn (TokenOCurBracket (AlexPn a l c) ) = show(l) ++ ":" ++ show(c) ++ " -> " ++ "}"
token_posn (TokenIf (AlexPn a l c) ) = show(l) ++ ":" ++ show(c) ++ " -> " ++ "if"
token_posn (TokenThen (AlexPn a l c) ) = show(l) ++ ":" ++ show(c) ++ " -> " ++ "then"
token_posn (TokenElse (AlexPn a l c) ) = show(l) ++ ":" ++ show(c) ++ " -> " ++ "else"
token_posn (TokenFor (AlexPn a l c) ) = show(l) ++ ":" ++ show(c) ++ " -> " ++ "for"
token_posn (TokenCreate (AlexPn a l c) ) = show(l) ++ ":" ++ show(c) ++ " -> " ++ "create"
token_posn (TokenImport (AlexPn a l c) ) = show(l) ++ ":" ++ show(c) ++ " -> " ++ "import"
token_posn (TokenBlank (AlexPn a l c) ) = show(l) ++ ":" ++ show(c) ++ " -> " ++ "blank"
token_posn (TokenFlipX (AlexPn a l c) ) = show(l) ++ ":" ++ show(c) ++ " -> " ++ "flipx"
token_posn (TokenFlipY (AlexPn a l c) ) = show(l) ++ ":" ++ show(c) ++ " -> " ++ "flipy"
token_posn (TokenRotate (AlexPn a l c) ) = show(l) ++ ":" ++ show(c) ++ " -> " ++ "rotate"
token_posn (TokenSubtile (AlexPn a l c) ) = show(l) ++ ":" ++ show(c) ++ " -> " ++ "subtile"
token_posn (TokenConj (AlexPn a l c) ) = show(l) ++ ":" ++ show(c) ++ " -> " ++ "conj"
token_posn (TokenNeg (AlexPn a l c) ) = show(l) ++ ":" ++ show(c) ++ " -> " ++ "neg"
token_posn (TokenSupersize (AlexPn a l c) ) = show(l) ++ ":" ++ show(c) ++ " -> " ++ "supersize"
token_posn (TokenStack (AlexPn a l c) ) = show(l) ++ ":" ++ show(c) ++ " -> " ++ "stack"
token_posn (TokenJoin (AlexPn a l c) ) = show(l) ++ ":" ++ show(c) ++ " -> " ++ "join"
token_posn (TokenAND (AlexPn a l c) ) = show(l) ++ ":" ++ show(c) ++ " -> " ++ "&&"
token_posn (TokenOR (AlexPn a l c) ) = show(l) ++ ":" ++ show(c) ++ " -> " ++ "||"
token_posn (TokenNOT (AlexPn a l c) ) = show(l) ++ ":" ++ show(c) ++ " -> " ++ "not"
token_posn (TokenInt (AlexPn a l c) ) = show(l) ++ ":" ++ show(c)
token_posn (TokenTile (AlexPn a l c) ) = show(l) ++ ":" ++ show(c)
token_posn (TokenTileG (AlexPn a l c) ) = show(l) ++ ":" ++ show(c)
token_posn (TokenBool (AlexPn a l c) ) = show(l) ++ ":" ++ show(c)
token_posn (TokenBTrue (AlexPn a l c) ) = show(l) ++ ":" ++ show(c) ++ " -> " ++ "true"
token_posn (TokenBFalse (AlexPn a l c) ) = show(l) ++ ":" ++ show(c) ++ " -> " ++ "false"
token_posn (TokenGroup (AlexPn a l c) g) = show(l) ++ ":" ++ show(c) ++ " -> " ++ "tile group - " ++ g 
token_posn (TokenVar (AlexPn a l c) v) = show(l) ++ ":" ++ show(c) ++ " -> " ++ "variable - " ++ v
token_posn (TokenPrint (AlexPn a l c) ) = show(l) ++ ":" ++ show(c) ++ " -> " ++ "print\n" ++ "Statement must be written at the end of the program."
token_posn (TokenNCol (AlexPn a l c) ) = show(l) ++ ":" ++ show(c) ++ " -> " ++ "numCol"
token_posn (TokenNRow (AlexPn a l c) ) = show(l) ++ ":" ++ show(c) ++ " -> " ++ "numRow"
token_posn (TokenSemiColon (AlexPn a l c) ) = show(l) ++ ":" ++ show(c) ++ " -> " ++ ";"
token_posn (TokenHash (AlexPn a l c) ) = show(l) ++ ":" ++ show(c) ++ " -> " ++ "#"




}