module IR where

-- for simplicity, we use the same binary operators
-- as in theabstract syntax
import AST

-- temporaries and labels are just strings
type Temp  = String
type Label = String

data Instr = MOVE Temp Temp                -- t1 := t2
  | MOVEI Temp Int                -- t  := n
  | OP BiOp Temp Temp Temp       -- t3 := t1 op t2
  | OPI BiOp Temp Temp Int       -- t2 := t1 op n
  | PRINT Temp      -- t3 := t1 op t2
  | LABEL Label                   -- define label
  | MAINFUN Int                   -- define label
  | JUMP Label                    -- unconditional jump
  | COND Temp ReOp Temp Label Label  -- conditional jump
  | RETURN
  deriving (Show)