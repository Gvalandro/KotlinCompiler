module Mips where

import IR
import AST

-- Função principal que traduz uma lista de instruções intermediárias para código MIPS
translateProgram :: [Instr] -> [String]
translateProgram = concatMap toMips

-- Traduz uma única instrução intermediária para uma lista de instruções MIPS
toMips :: Instr -> [String]
toMips (MAINFUN _) = []
toMips (MOVE t1 t2)
    | head t2 == '$' = ["\tmove " ++ reg t1 ++ ", " ++ t2]
    | otherwise = ["\tmove " ++ reg t1 ++ ", " ++ reg t2]
toMips (MOVEI t val) = ["\tli " ++ reg t ++ ", " ++ show val]
toMips (OP Add t1 t2 t3) = ["\tadd " ++ reg t1 ++ ", " ++ reg t2 ++ ", " ++ reg t3]
toMips (OP Sub t1 t2 t3) = ["\tsub " ++ reg t1 ++ ", " ++ reg t2 ++ ", " ++ reg t3]
toMips (OP Mult t1 t2 t3) = ["\tmul " ++ reg t1 ++ ", " ++ reg t2 ++ ", " ++ reg t3]
toMips (OP Div t1 t2 t3) = ["\tdiv " ++ reg t2 ++ " , " ++ reg t3, "\tmflo " ++ reg t1]
toMips (OP Mod t1 t2 t3) = ["\tdiv " ++ reg t2 ++ " , " ++ reg t3, "\tmfhi " ++ reg t1]
toMips (OPI Add t1 t2 val) = ["\taddi " ++ reg t1 ++ ", " ++ reg t2 ++ ", " ++ show val]
toMips (OPI Sub t1 t2 val) = ["\tsubi " ++ reg t1 ++ ", " ++ reg t2 ++ ", " ++ show val]
toMips (OPI Mult t1 t2 val) = ["\tmul " ++ reg t1 ++ ", " ++ reg t2 ++ ", " ++ show val]
toMips (OPI Div t1 t2 val) = ["\tdiv " ++ reg t2 ++ " , " ++ show val, "\tmflo " ++ reg t1]
toMips (OPI Mod t1 t2 val) = ["\tdiv " ++ reg t2 ++ " , " ++ show val, "\tmfhi " ++ reg t1]
toMips (LABEL lbl) = [lbl ++ ":"]
toMips (JUMP lbl) = ["\tj " ++ lbl]
toMips (COND t1 op t2 lblTrue lblFalse) = 
    let mipsCond = translateReOp op
    in [mipsCond ++ " " ++ reg t1 ++ ", " ++ reg t2 ++ ", " ++ lblTrue, "\tj " ++ lblFalse]
toMips (PRINT t) = ["\tli $v0, 1", "\tmove $a0, " ++ reg t, "\tsyscall"]


-- Tradução de operadores relacionais para MIPS
translateReOp :: ReOp -> String
translateReOp GreaterThan = "bgt"
translateReOp LessThan = "blt"
translateReOp Equal = "beq"
translateReOp NotEqual = "bne"
translateReOp GreaterEqual = "bge"
translateReOp LessEqual = "ble"

-- Mapeia os registradores temporários para nomes em MIPS
reg :: String -> String
reg t = "$t" ++ drop 1 t  -- Supõe que os temporários têm nomes como "t1", "t2", etc.
