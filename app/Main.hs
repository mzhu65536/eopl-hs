module Main where

import Direct.Interpreter.Eval
import Parser.Parser
import System.Console.Haskeline

{-
EOPL Interpreter REPL 
-}


main :: IO ()
main = runInputT defaultSettings readEvalPrintLoop

readEvalPrintLoop :: InputT IO ()
readEvalPrintLoop = do
  maybeLine <- getInputLine "\ESC[1;32m\STXλ \ESC[0m\STX"
  case maybeLine of
       Nothing -> return () 
       Just "exit" -> return ()
       Just line -> do parsedLine <- return (parseExpr line)
                       outputStrLn $
                         case parsedLine of
                           Left err -> "Parsing Error: " ++ err
                           Right ast -> (show $ eval ast)
                       readEvalPrintLoop
    
