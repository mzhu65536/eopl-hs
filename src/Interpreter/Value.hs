module Interpreter.Value where

import Interpreter.Data
import Interpreter.Storage
import Parser.Syntax

import Control.Monad.Writer
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Except

procedure :: [Sym] -> Exp -> Eval Ref
procedure syms expr = do env <- ask
                         extendSto $ VClosure syms expr env
