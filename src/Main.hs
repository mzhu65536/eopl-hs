module Main where

import Interpreter.Eval
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
       Just line -> do case parseExpr line of
                         Left err -> outputStrLn $ show err
                         Right ast -> return (eval2 ast) >>
                                      outputStr ""
                       readEvalPrintLoop
    
