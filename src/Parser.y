{

module Parser where

import Lexer
import AST

}

%name parser
%tokentype { Token }
%error { parseError }

%token
id              { ID $$ }
num             { INTEGER $$ }
'true'          { BOOLEAN_LIT True }
'false'         { BOOLEAN_LIT False }
'('             { LPAREN }
')'             { RPAREN }
'{'             { LBRACE }
'}'             { RBRACE }
':'             { COLON }
'='             { ASSIGN }
'++'            { INCREMENT } 
'--'            { DECREMENT } 
'+'             { PLUS }
'-'             { MINUS }
'*'             { TIMES }
'/'             { DIVIDE }
'%'             { MOD_TOK }
'=='            { EQUAL }
'!='            { NEQ }
'<'             { LT_TOK }
'<='            { LE_TOK }
'>'             { GT_TOK }
'>='            { GE_TOK }
'&&'            { AND }
'||'            { OR }
'!'             { NOT_TOK }
'fun'           { FUN }
'main'          { MAIN }
'val'           { VAL }
'var'           { VAR }
'print'         { PRINT }  
'readln'        { READLINE } 
'if'            { IF }
'else'          { ELSE }
'while'         { WHILE }

-- Types
'Int'           { INT }
'Boolean'       { BOOL }


-- precedência dos operadores 
%left '||'                              -- operador lógico OR
%left '&&'                              -- operador lógico AND
%left '==' '!='                         -- operadores de igualdade
%left '<' '<=' '>' '>='                 -- operadores de comparação
-- %right '=' '+=' '-=' '*=' '/='       -- operadores de atribuição (MENOR PRECEDENCIA)
%left '+' '-'                           -- operadores de adição e subtração
%left '*' '/' '%'                       -- operadores de multiplicação, divisão e resto
%right '!' '++' '--'                    -- operadores unários (MAIOR PRECEDENCIA)

%%

-- regra principal do programa
Program         : Func                                                      { Program $1 }

Func            : 'fun' 'main' '(' ')' '{' Declares StmtList '}'            { DeclMain $6 $7 }

StmtList        : Stmt StmtList                                             { $1 : $2 }
                | {- empty -}                                               { [] }

-- statements
Stmt            : 'if' '(' Exp ')' Stmt 'else' Stmt 			    { IfElse $3 $5 $7 }
                | 'if' '(' Exp ')' Stmt                                     { If $3 $5 }
                | 'while' '(' Exp ')' Stmt                                  { While $3 $5 }
                | '{' StmtList '}'                                          { StmtLi $2 }
                | 'print' '(' Exp ')'                                       { Print $3 }
                | id '=' Exp                                                { Assign $1 $3 }
                | id '++'                                                   { PostAdd $1}
                | id '--'                                                   { PostMinus $1}
                | Exp                                                       { ExpStmt $1 }
	       
Declares        : Declare Declares                                          { $1 : $2 }
                | {- empty -}                                               { [] }


Declare         : 'val' id ':' Tp                                           { ValDecl $2 $4 }    -- Declara a variável sem inicializa-a, tipo é necessário              
                | 'var' id ':' Tp                                           { VarDecl $2 $4 }    -- Declara a variável sem inicializa-a, tipo é necessário 


-- definição de tipo
Tp              : 'Int'                                                     { TpInt }
                | 'Boolean'                                                 { TpBool }

-- expressões aritméticas e booleanas
Exp             : num                                                       { Num $1 }
                | id                                                        { Var $1 }
                | 'true'                                                    { BoolLit True }    
                | 'false'                                                   { BoolLit False }
                | '!' Exp                                                   { Not $2 }
                | Exp ReOp Exp                                              { ReOp $1 $2 $3 }
                | Exp BiOp Exp                                              { BiOp $1 $2 $3 }
                | 'readln' '(' ')'                                          { ReadLine }
                | '(' Exp ')'                                               { $2 }

ReOp : '||'      { Or }
    | '&&'      { And }
    | '=='      { Equal }               -- '=='
    | '!='      { NotEqual }                 -- '!='
    | '>='      { GreaterEqual }        -- '>='
    | '>'       { GreaterThan }               -- '>'
    | '<='      { LessEqual }          -- '<='
    | '<'       { LessThan }                 -- '<'

BiOp : '+'     { Add }                         -- '+' 
    | '-'       { Sub }                       -- '-'
    | '*'       { Mult }                        -- '*'
    | '/'       { Div }                         -- '/'
    | '%'       { Mod }                         -- '%'

{

parseError :: [Token] -> a
parseError tokens = error $ "Parse error at tokens: " ++ show tokens

}
