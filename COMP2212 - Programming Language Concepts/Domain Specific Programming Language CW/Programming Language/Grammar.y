{
module Grammar where
import Tokens
}

%name parseCalc 
%tokentype { TokenType } 
%error { parseError }   

%token
    
    '-'                     { TokenMinus _}
    '*'                     { TokenMult _}
    '/'                     { TokenDiv _}
    '('                     { TokenOpBracket _}
    ')'                     { TokenClBracket _}
    '='                     { TokenEq _}
    '+'                     { TokenPlus _}
    '<'                     { TokenLess _}
    "=="                    { TokenEquality _}
    ','                     { TokenComma _}
    '['                     { TokenOSBracket _}
    ']'                     { TokenClSBracket _}
    '{'                     { TokenOCurBracket _}
    '}'                     { TokenClCurBracket _}
    ';'                     { TokenSemiColon _ }
    '#'                     { TokenHash _ }
    import                  { TokenImport _ }
    print                   { TokenPrint _ }
    if                      { TokenIf _}
    then                    { TokenThen _}
    else                    { TokenElse _}
    for                     { TokenFor _}
    create                  { TokenCreate _}
    blank                   { TokenBlank _}
    flipx                   { TokenFlipX _}
    flipy                   { TokenFlipY _ }
    numCol                  { TokenNCol _}
    numRow                  { TokenNRow _}
    rotate                  { TokenRotate _}
    subtile                 { TokenSubtile _}
    conj                    { TokenConj _}
    neg                     { TokenNeg _}
    supersize               { TokenSupersize _}
    stack                   { TokenStack _}
    join                    { TokenJoin _}
    "&&"                    { TokenAND _}
    "||"                    { TokenOR _}
    not                     { TokenNOT _}
    true                    { TokenBTrue _}
    false                   { TokenBFalse _}
    tokGr                   { TokenGroup _ $$ }
    var                     { TokenVar _ $$ }
    int                     { TokenDigit _ $$ }

%right for
%nonassoc '=' 
%nonassoc '<' "=="
%left "||"
%left "&&" 
%left '+' '-'
%left '*' '/'


%%
Start : import var ';' Start   {Imports $2 $4}
      | End                    {$1}
     
End : Ent ';' End                {Semis $1 $3}
    | print '(' Func ')' ';'     {Print $3}

Loop : for  '(' Math ')' '{' Loop '}'    {For $3 $6}
     | HashList                          {$1}

HashList : '#' Ent HashList          {Hashes $2 $3}
         | '#' Ent                   {Hash $2}

Ent : var '=' Func                                          {Assign $1 $3}
    | '[' tokGr ']' '=' '[' List ']'                        {TGroup $2 $6} 
    | for  '(' Math ')' '{' Loop '}'                        {For $3 $6}
    | if '(' Operations ')' then '{' Ent '}' IfStatement 	{If $3 $7 $9}
    | Func                                                  {$1}

Func : create '[' Row ']'                                       {Tile $3}
     | blank '(' Prim ',' Prim ')'                              {Blank $3 $5}
     | numCol '(' Func ')'                                      {NumCol $3}
     | numRow '(' Func ')'                                      {NumRow $3}
     | flipy '(' Func ')'                                       {FlipY $3}
     | flipx '(' Func ')'                                       {FlipX $3}
     | join '[' List ']'                                        {Join $3}
     | stack '[' List ']'                                       {Stack $3}
     | rotate '(' Func ')'                                      {Rotate $3}
     | subtile '(' Func ',' '(' Prim ',' Prim ')' ',' Prim ')'  {Sub $3 $6 $8 $11}
     | supersize '(' Func ',' Prim ')'                          {Super $3 $5}
     | tokGr '[' Prim ']'                                       {Accessor $1 $3} 
     | BoolTiles                                                {$1}

BoolTiles :   conj '(' Func ',' Func ')'                        {Conjunction $3 $5}
	        | neg '(' Func ')'                                  {Negation $3}
	        | Math                                              {$1}

IfStatement :   else if '(' Operations ')' then '{' Ent '}' IfStatement   {ElseIf $4 $8 $10}
              | else '{' Ent '}'                          		       {Else $3}

List :   var                    {Single $1}
	   | var ',' List           {List $1 $3}

Row :   '[' Block ']'               {Row $2}
	  | '[' Block ']' ',' Row       {Rows $2 $5} -- this could change to only row or go into block 

Block :   int 		  	    {Block $1}
        | int ',' Block     {Blocks $1 $3}


Operations : '(' Boolean ')' "&&" '(' Boolean ')'         {And $2 $6}
	       | '(' Boolean ')' "||" '(' Boolean ')'         {Or $2 $6}
	       | not '(' Operations ')'                       {Not $3} 
	       | Boolean                                      {$1}

Boolean : Math '<' Math                         {Less $1 $3}
	    | Math "==" Math                        {Equality $1 $3}
	    | true                                  {BTrue}
        | false                                 {BFalse} 

Math :   Math '+' Math          {Add $1 $3}
         | Math '-' Math        {Minus $1 $3}
         | Math '*' Math        {Mult $1 $3}
         | Math '/' Math        {Div  $1 $3}
         | '(' Math ')'         {$2} 
         | Prim                 {$1}

Prim :   int                    {Int $1} 
       | var                    {Var $1} 
     

{ 
parseError :: [TokenType] -> a
parseError rem | null rem = error "Missing input"
               | otherwise = error ("Parse error on: " ++ (token_posn $ head rem))
parseError _ = error "Parse error" 


data Exp = Assign String Exp
         | TGroup String Exp
         | Accessor String Exp
         | For Exp Exp
         | If Exp Exp Exp
         | ElseIf Exp Exp Exp
         | Else Exp
         | Tile Exp
         | Blank Exp Exp
         | NumCol Exp
         | NumRow Exp
         | Imports String Exp
         | Print Exp
         | FlipY Exp
         | FlipX Exp
         | Join Exp
         | Stack Exp
         | Rotate Exp
         | Sub Exp Exp Exp Exp
         | Super Exp Exp
         | Conjunction Exp Exp
         | Negation Exp
         | Var String
         | Single String
         | List String Exp
         | Row Exp
         | Rows Exp Exp
         | Block Int
         | Blocks Int Exp  
         | And Exp Exp
         | Or Exp Exp
         | Not Exp
         | Less Exp Exp
         | Equality Exp Exp
         | Add Exp Exp
         | Mult Exp Exp
         | Minus Exp Exp
         | Div Exp Exp
         | Int Int
         | BTrue 
         | BFalse
         | Semis Exp Exp
         | Hash Exp
         | Hashes Exp Exp
         deriving (Show, Eq, Read)
         
data Type =   TGType
            | TileType
            | RowType
            | IntType
            | BoolType
            | BlockType
            | FunctionType Type Type
            | PairType Type Type
            | HashType Type 
            | HashList Type Type 
            | SemiList Type Type
            deriving (Show, Eq, Read)
} 