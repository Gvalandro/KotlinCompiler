module CodeGen where

import IR
import AST
import SymbolTable
import           Data.Map (Map)
import qualified Data.Map as Map
import           Control.Monad.State (State)
import qualified Control.Monad.State as State
import Control.Monad.Trans.State.Lazy (State, runState)

type Supply = (Int,Int) 
type Table = [(String, Temp)]

createTemps :: [Env] -> Table
createTemps ((x,_):xs) = [(x,"t"++show (length xs))] ++ createTemps xs

newTemp :: State Supply Temp
newTemp 
  = do (temps,labels) <- State.get
       State.put (temps+1, labels)
       return ("t"++show temps)

newLabel :: State Supply Label 
newLabel
  = do (temps,labels) <- State.get
       State.put (temps, labels+1)
       return ("L"++show labels)

findTemp :: String -> Table -> Temp
findTemp name [] = error "invalid variable"
findTemp name ((x,y):xs)
    | x == name = y
    | otherwise = findTemp name xs

newTemps :: Int -> State Supply [Temp]
newTemps n 
    | n > 0 = do t <- newTemp
                 ts <- newTemps (n-1)
                 return (t:ts)
    | otherwise = return []
    

reuseTemps :: Int -> State Supply ()
reuseTemps n
  = do (temps, labels) <- State.get
       State.put (temps-n, labels)


transExpr :: Exp -> Table -> Temp -> State Supply [Instr]
transExpr (Var x) tabl dest = return [MOVE dest (findTemp x tabl)]
transExpr (Num n) table dest = return [MOVEI dest n]
transExpr (BiOp e1 op e2) table dest
    = do temp1 <- newTemp 
         temp2 <- newTemp 
         code1 <- transExpr e1 table temp1 
         code2 <- transExpr e2 table temp2
         reuseTemps 2
         return (code1 ++ code2 ++ [OP op dest temp1 temp2])
transExpr (BoolLit True) _ dest = return [MOVEI dest 1] 
transExpr (BoolLit False) _ dest = return [MOVEI dest 0]
transExpr (ReOp e1 op e2) table dest
    = do label1 <- newLabel
         label2 <- newLabel
         label3 <- newLabel
         code1 <- transCond (ReOp e1 op e2) table label1 label2
         return (code1 ++ [LABEL label1, MOVEI dest 0, JUMP label3] ++ [LABEL label2, MOVEI dest 1, JUMP label3])




transArgs :: [Exp] -> Table -> State Supply ([Instr], [Temp])
transArgs [] tabl = return ([], [])
transArgs (exp:exps) tabl
        = do temp <- newTemp 
             code <- transExpr exp tabl temp 
             (code', temps') <- transArgs exps tabl
             return (code++code', temp:temps')




transStm :: Stmt -> Table -> State Supply [Instr]
transStm (Assign var expr) tabl = transExpr expr tabl (findTemp var tabl)
transStm (Return) table = return [RETURN]
transStm (StmtLi stmts) tabl = transStmList stmts tabl
transStm (Print expr) tabl = do
  dest <- newTemp
  code <- transExpr expr tabl dest 
  return $ code ++ [PRINT dest ]
transStm (If cond stm) tabl 
    = do label1  <- newLabel 
         label2 <- newLabel 
         code1  <- transCond cond tabl label1 label2 
         code2  <- transStm stm tabl
         return (code1 ++ [LABEL label1] ++ code2 ++ [LABEL label2])
transStm (IfElse cond stm1 stm2) tabl
    = do label1 <- newLabel 
         label2 <- newLabel 
         label3 <- newLabel 
         code1 <- transCond cond tabl label1 label2 
         code2 <- transStm stm1 tabl 
         code3 <- transStm stm2 tabl 
         return (code1 ++ [LABEL label1] ++ code2 ++ [JUMP label3, LABEL label2] ++ code3 ++ [LABEL label3])
transStm  (While cond stm) tabl 
    = do label1 <- newLabel
         label2 <- newLabel
         label3 <- newLabel
         code1 <- transCond cond tabl label2 label3
         code2 <- transStm stm tabl 
         return ([LABEL label1] ++ code1 ++ [LABEL label2] ++ code2 ++ [JUMP label1, LABEL label3])
transStm (ExpStmt expr) tabl
    = do dest <- newTemp
         code <- transExpr expr tabl dest
         return code
transStm (PostAdd (x)) tabl
    = do dest1 <- newTemp
         code1 <- transExpr (Num 1) tabl dest1
         return (code1 ++ [OP Add (findTemp x tabl) (findTemp x tabl) dest1])
transStm (PostMinus (x)) tabl
    = do dest1 <- newTemp
         code1 <- transExpr (Num 1) tabl dest1
         return (code1 ++ [OP Sub (findTemp x tabl) (findTemp x tabl) dest1])
--transStm (Print id) tabl =  return  [CALLPRINT (findTemp (show(id)) tabl)]
        

transCond :: Exp -> Table -> Label -> Label -> State Supply [Instr]
transCond (ReOp e1 And e2) tabl ltrue lfalse
    = do label2 <- newLabel
         code1 <- transCond e1 tabl label2 lfalse
         code2 <- transCond e2 tabl ltrue lfalse 
         return ( code1 ++ [LABEL label2] ++ code2 )
transCond (ReOp e1 Or e2) tabl ltrue lfalse
    = do label2 <- newLabel
         code1 <- transCond e1 tabl ltrue label2
         code2 <- transCond e2 tabl ltrue lfalse 
         return ( code1 ++ [LABEL label2] ++ code2 )
transCond (ReOp e1 rel e2) tabl ltrue lfalse
    | rel == LessThan || rel == LessEqual || rel == GreaterThan || rel == GreaterEqual || rel == NotEqual || rel == Equal =
        do temp1 <- newTemp
           temp2 <- newTemp 
           code1 <- transExpr e1 tabl temp1
           code2 <- transExpr e2 tabl temp2
           return ( code1 ++ code2 ++ [COND temp1 rel temp2 ltrue lfalse] )
transCond (BoolLit True) tabl ltrue lfalse = return [JUMP ltrue]
transCond (BoolLit False) tabl ltrue lfalse = return [JUMP lfalse]
transCond (Not e1) tabl ltrue lfalse = (transCond e1 tabl lfalse ltrue)
transCond (Var x) tabl ltrue lfalse
    = do t <- newTemp
         code1 <- transExpr (Var x) tabl t
         return ( code1 ++ [COND t NotEqual "zero" ltrue lfalse] )



transStmList :: [Stmt] -> Table -> State Supply [Instr]
transStmList [] tabl = return []
transStmList (stm:rest) tabl 
    = do code1 <- transStm stm tabl 
         code2 <- transStmList rest tabl
         return (code1 ++ code2)




-- -- Corrigindo myZip2
myZip2 :: [Declare] -> [Temp] -> Table
myZip2 [] _ = []
myZip2 ((ValDecl name _):xs) (state:supply) = (name, state) : myZip2 xs supply
myZip2 ((VarDecl name _):xs) (state:supply) = (name, state) : myZip2 xs supply


transProgram :: Program -> State Supply [Instr]
transProgram (Program (DeclMain decls body)) 
    = do tlocals <- newTemps (length decls) 
         let table = myZip2 decls tlocals
         code <- transStmList body table
         return (MAINFUN (length table) : code)