module AST where

data Program 
    = Program Func
    deriving (Eq, Show)

data Func
    = DeclMain [Declare] [Stmt]
    deriving (Eq, Show)

data Stmt
    = IfElse Exp Stmt Stmt
    | If Exp Stmt
    | While Exp Stmt
    | StmtLi [Stmt]
    | Print Exp
    | Assign String Exp         -- não deveria ser ID exp?   -- Atribuição simples, e.g., `x = 5`
    | PostAdd String
    | PostMinus String
    | ExpStmt Exp              -- Standalone expression as a statement, e.g., `x + 1`
    | Return 
    deriving (Eq, Show)

data Tp 
    = TpInt
    | TpBool
    deriving (Eq, Show)

data Declare
    = ValDecl String Tp
    | VarDecl String Tp
    deriving (Eq, Show)

data Exp
    = Num Int                  -- Literal inteiro, e.g., `42`
    | DoubleLit Double         -- Literal double, e.g., `3.14`
    | BoolLit Bool             -- Literal booleano, e.g., `true` ou `false`   --devemos fazer um separado para true/false
    | Not Exp                  -- Operador NOT lógico, e.g., `!x`
    | BiOp Exp BiOp Exp
    | ReOp Exp ReOp Exp
    | Var String               -- Chamada de variável, e.g., x
    | ReadLine 		       -- Funcao Read
    deriving (Eq, Show)

data ReOp =  Or 
    | And 
    | Equal                -- '=='
    | NotEqual                  -- '!='
    | GreaterEqual         -- '>='
    | GreaterThan                -- '>'
    | LessEqual           -- '<='
    | LessThan 
    deriving (Eq, Show)

data BiOp = Add                         -- '+' 
    | Sub                       -- '-'
    | Mult                        -- '*'
    | Div                         -- '/'
    | Mod
    deriving (Eq, Show)