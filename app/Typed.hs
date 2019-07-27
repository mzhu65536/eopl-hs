module Typed where

import General.Eval
import General.Environment
import Type.Type
import Parser.Typed.Parser
import System.Console.Haskeline

import Control.Monad.Reader

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
       Just line -> do let parsedLine = parseExprTyped line
                       case parsedLine of
                         Left err -> outputStrLn $ "Parsing Error: " ++ err
                         Right ast -> do x <- liftIO (runEval' ast)
                                         outputStrLn $
                                           case x of
                                             Right ans -> show ans
                                             Left err  -> "Error: " ++ err
                       readEvalPrintLoop

