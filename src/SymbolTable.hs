module SymbolTable where

import Parser
import AST
--import qualified Data.Map as Map


type Env = (String, Tp)

typeVerify :: Program -> Bool
typeVerify x = callAnalysis (extract x) (functionList x)

extract :: Program -> [Env]
extract (Program (DeclMain decl _)) = extractDecls decl

extractDecls :: [Declare] -> [Env]
extractDecls [] = []
extractDecls ((ValDecl id ty):xs) = (id, ty):extractDecls xs
extractDecls ((VarDecl id ty):xs) = (id, ty):extractDecls xs

functionList :: Program -> [Stmt]
functionList (Program (DeclMain _ x)) = x

callAnalysis :: [Env] -> [Stmt] -> Bool
callAnalysis x y = checkBlocks x y

findType :: [Env] -> String -> Tp
findType [] name = error ("undeclared var " ++ name)
findType ((x,y):xs) name
    | x == name = y
    | otherwise = findType xs name

checkBlocks :: [Env] -> [Stmt] -> Bool
checkBlocks env [] = True
checkBlocks env (x:xs) = (checkStm env x) && (checkBlocks env xs)

checkStm :: [Env] -> Stmt -> Bool
checkStm env (Return) = True
checkStm env (If cond stm1) 
  | (checkExp env cond) == TpBool && (checkStm env stm1) == True = True
  | otherwise = error ("type error in if")
checkStm env (IfElse cond stm1 stm2) 
  | (checkExp env cond) == TpBool && (checkStm env stm1) == True && (checkStm env stm2) == True = True
  | otherwise = error ("type error in if then else")
checkStm env (While cond stm)
  | ((checkExp env cond) == TpBool) && (checkStm env stm) = True
  | otherwise = error "type error in while"
checkStm env (StmtLi stms)
  | (checkBlocks env stms) = True
  | otherwise = error "type error in block"
checkStm env (Print expr) 
  | (checkExp env expr) == TpInt || (checkExp env expr) == TpBool = True
  | otherwise = error "type error in Print"
checkStm env (Assign id expr)
  | ((checkExp env expr) == (findType env id)) = True
  | otherwise = error ("type error in assign")
checkStm env (PostAdd expr)
  | (findType env expr) == TpInt = True
  | otherwise = error "type error in ++ (increment)"
checkStm env (PostMinus expr)
  | (findType env expr) == TpInt = True
  | otherwise = error "type error in ++ (increment)"
checkStm env (ExpStmt expr)
  | (checkExp env expr) == TpInt || (checkExp env expr) == TpBool = True
  | otherwise = error "type error in ExpSimple"




checkExp :: [Env] -> Exp -> Tp
checkExp env (Num n) = TpInt
checkExp env (Var n) = findType env n
checkExp env (BoolLit n) = TpBool
checkExp env (Not expr)
  | (checkExp env expr) == TpBool = TpBool
  | otherwise = error "type error in + (biop error)"
checkExp env (BiOp e1 _ e2)
  | (checkExp env e1) == TpInt && (checkExp env e2) == TpInt = TpInt
  | otherwise = error "type error in + (biop error)"
checkExp env (ReOp e1 LessThan e2)
  | (checkExp env e1) == TpInt && (checkExp env e2) == TpInt = TpBool
  | otherwise = error "type error in < (lower than)"
checkExp env (ReOp e1 LessEqual e2)
  | (checkExp env e1) == TpInt && (checkExp env e2) == TpInt = TpBool
  | otherwise = error "type error in <= (lower or equals than)"
checkExp env (ReOp e1 GreaterThan e2)
  | (checkExp env e1) == TpInt && (checkExp env e2) == TpInt = TpBool
  | otherwise = error "type error in > (greater than)"
checkExp env (ReOp e1 GreaterEqual e2)
  | (checkExp env e1) == TpInt && (checkExp env e2) == TpInt = TpBool
  | otherwise = error "type error in > (greater than)"
checkExp env (ReOp e1 Equal e2)
  | (checkExp env e1) == TpInt && (checkExp env e2) == TpInt = TpBool
  | otherwise = error "type error in == (equals than)"
checkExp env (ReOp e1 NotEqual e2)
  | (checkExp env e1) == TpInt && (checkExp env e2) == TpInt = TpBool
  | otherwise = error "type error in != (diff than)"
checkExp env (ReOp e1 And e2)
  | (checkExp env e1) == TpBool && (checkExp env e2) == TpBool = TpBool
  | otherwise = error "type error in && (and)"
checkExp env (ReOp e1 Or e2)
  | (checkExp env e1) == TpBool && (checkExp env e2) == TpBool = TpBool
  | otherwise = error "type error in || (or)"
checkExp env x = error (show x)