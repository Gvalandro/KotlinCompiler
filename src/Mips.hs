module Mips where

import IR 
import AST

-- Função principal que traduz uma lista de instruções intermediárias para código MIPS
translateProgram :: [Instr] -> [String]
translateProgram = concatMap translateInstr

-- Traduz uma única instrução intermediária para uma lista de instruções MIPS
translateInstr :: Instr -> [String]
translateInstr (MOVEI t val) = ["li " ++ reg t ++ ", " ++ show val]
translateInstr (MOVE t1 t2) = ["move " ++ reg t1 ++ ", " ++ reg t2]
translateInstr (PRINT t)    = ["li $v0, 1", "move $a0, " ++ reg t, "syscall"]
translateInstr (LABEL lbl)  = [lbl ++ ":"]
translateInstr (JUMP lbl)   = ["j " ++ lbl]
translateInstr (COND t1 op t2 lblTrue lblFalse) =
  let mipsCond = translateReOp op
  in  [mipsCond ++ " " ++ reg t1 ++ ", " ++ reg t2 ++ ", " ++ lblTrue, "j " ++ lblFalse]
translateInstr (OPI op t1 t2 val) = case op of
  Add -> ["addi " ++ reg t2 ++ ", " ++ reg t1 ++ ", " ++ show val]
  Sub -> ["subi " ++ reg t2 ++ ", " ++ reg t1 ++ ", " ++ show val]
  Mult -> ["mul " ++ reg t2 ++ ", " ++ reg t1 ++ ", " ++ show val]
  Div -> ["div " ++ reg t1 ++ ", " ++ show val, "mflo " ++ reg t2]  
translateInstr (OP op t1 t2 t3) = case op of
  Add -> ["add " ++ reg t3 ++ ", " ++ reg t1 ++ ", " ++ reg t2]
  Sub -> ["sub " ++ reg t3 ++ ", " ++ reg t1 ++ ", " ++ reg t2]
  Mult -> ["mul " ++ reg t3 ++ ", " ++ reg t1 ++ ", " ++ reg t2]
  Div -> ["div " ++ reg t1 ++ ", " ++ reg t2, "mflo " ++ reg t3] 
translateInstr _ = [] -- Caso precise lidar com outras instruções no futuro

-- Tradução de operadores relacionais para MIPS
translateReOp :: ReOp -> String
translateReOp GreaterThan = "bgt"
translateReOp LessThan    = "blt"
translateReOp Equal       = "beq"
translateReOp NotEqual    = "bne"
translateReOp GreaterEqual = "bge"
translateReOp LessEqual    = "ble"
translateReOp And = error "Operador And não pode ser traduzido diretamente no MIPS"
translateReOp Or  = error "Operador Or não pode ser traduzido diretamente no MIPS"

-- Mapeia os registradores temporários para nomes em MIPS
reg :: String -> String
reg t = "$t" ++ drop 1 t  -- Supõe que os temporários têm nomes como "t1", "t2", etc.
