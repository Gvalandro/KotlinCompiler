{

module Lexer where

}


%wrapper "basic"

-- Definições de padrões
$digit      = [0-9]
$alpha      = [a-zA-Z]
$alphanum   = [a-zA-Z0-9]


tokens :-
  $white+                             ;   -- Ignorar espaços em branco

  -- Comentários
  "//".*                              ;   -- Comentário de linha
  "/*"(.|\n)*"*/" 		                ;   -- Comentário de bloco

  -- Delimitadores
  "("                                 { \_ -> LPAREN }
  ")"                                 { \_ -> RPAREN }
  "{"                                 { \_ -> LBRACE }
  "}"                                 { \_ -> RBRACE }
  ":"                                 { \_ -> COLON }
  
  -- Operadores Compostos
  "="                                 { \_ -> ASSIGN }
  "++"				      { \_ -> INCREMENT }
  "--"		            	      { \_ -> DECREMENT}

  -- Operadores Aritmeticos
  "+"			              { \_ -> PLUS}
  "-"				      { \_ -> MINUS}
  "*"			              { \_ -> TIMES}
  "/"			              { \_ -> DIVIDE }
  "%"			              { \_ -> MOD_TOK}


  -- Operadores de Comparação
  "=="                                { \_ -> EQUAL }
  "!="                                { \_ -> NEQ }
  "<"                                 { \_ -> LT_TOK }
  "<="                                { \_ -> LE_TOK }
  ">"                                 { \_ -> GT_TOK }
  ">="                                { \_ -> GE_TOK }
  "&&"                                { \_ -> AND }
  "||"                                { \_ -> OR }
  "!"                                 { \_ -> NOT_TOK }
  
  -- Literais
  $digit+                             { \s -> INTEGER (read s) }

  -- Identificadores e Palavras Reservadas
  $alpha $alphanum*                   { \s -> lexerText s }


  -- Caracteres inválidos
  .                                   { \c -> error ("Caractere inválido: " ++ show c) }

{
-- Tipo algébrico para tokens
data Token = ID String         -- e.g. xy123
           | LPAREN            -- (
           | RPAREN            -- )
           | LBRACE            -- {
           | RBRACE            -- }
	   | COLON	           -- :
           | ASSIGN            -- =
           | INCREMENT         -- ++
           | DECREMENT         -- --
           | PLUS              -- +
           | MINUS             -- -
           | TIMES             -- *
           | DIVIDE            -- /
	   | MOD_TOK	         -- %
           | EQUAL             -- ==
           | NEQ               -- !=
           | LT_TOK            -- <
           | LE_TOK            -- <=
           | GT_TOK            -- >
           | GE_TOK            -- >=
           | AND               -- &&
           | OR                -- ||
           | NOT_TOK           -- !
           | FUN               -- func
           | MAIN              -- main
           | VAL               -- val
           | VAR               -- var
	   | PRINT	       -- print
	   | READLINE	       -- readln
           | IF                -- if
           | ELSE              -- else
           | WHILE             -- while
           | INT	       -- Int type
           | INTEGER Int       -- integer literals
	   | BOOL	       -- Boolean Type
	   | BOOLEAN_LIT Bool  -- true / false
           deriving (Eq, Show)

-- Função auxiliar para distinguir palavras reservadas de identificadores
lexerText :: String -> Token
lexerText "if"     = IF
lexerText "else"   = ELSE
lexerText "while"  = WHILE
lexerText "fun"    = FUN
lexerText "main"   = MAIN
lexerText "val"    = VAL
lexerText "var"    = VAR
lexerText "Int"    = INT
lexerText "Boolean"= BOOL
lexerText "print"  = PRINT
lexerText "readln" = READLINE
lexerText "true"   = BOOLEAN_LIT True
lexerText "false"  = BOOLEAN_LIT False
lexerText xs       = ID xs
}
