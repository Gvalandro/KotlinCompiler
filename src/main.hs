import System.IO (getContents)
import Control.Monad.Trans.State.Lazy (evalState)
import Data.Functor.Identity (runIdentity)
import Lexer (alexScanTokens)
import Parser (parser)
import SymbolTable
import CodeGen 
import IR
import Mips (translateProgram)

import qualified Control.Monad.State as State
import Control.Monad.Trans.State.Lazy (State, runState)

initialSupply :: Supply
initialSupply = (0, 0)

runCodeGen :: State Supply [Instr] -> [Instr]
runCodeGen gen = State.evalState gen initialSupply


-- print a list of IR instructions
printIR :: [Instr] -> IO ()
printIR = mapM_ print

-- Função auxiliar para exibir uma lista de instruções MIPS
printMIPS :: [String] -> IO ()
printMIPS = mapM_ putStrLn

main :: IO ()
main = do
  -- Lê o código fonte da entrada padrão
  txt <- getContents
  
  -- Exibe o código fonte
  putStrLn "Código fonte:\n"
  putStrLn txt
  putStrLn "\n" 
  
  -- Realiza a análise léxica e sintática
  putStrLn "Realizando análise sintática..."
  let parseResult = parser $ alexScanTokens txt
  
  -- Mostra o AST gerado
  putStrLn "\nAST gerado:"
  print parseResult
  putStrLn "\n"

  -- testa os tipos
  putStrLn "\nAnalise de tipos:"
  let typeResult = typeVerify parseResult
  print typeResult

  putStrLn "\n"
  let ir = evalState (transProgram parseResult) initialSupply
  printIR ir
  
  putStrLn "\nCódigo gerado em MIPS:"
  let mipsCode = translateProgram ir
  mapM_ putStrLn mipsCode  

-- Gera o código intermediário executando o estado inicial
  --putStrLn "Gerando código intermediário..."
  --let intermediateCode = evalState (transProgram parseResult) initialSupply
  
  -- Mostra o código intermediário
  --putStrLn "\nCódigo intermediário:"
  --mapM_ print intermediateCode