module Interpreter.Value where

import Interpreter.Data
import Interpreter.Storage
import Parser.Syntax

procedure :: [Sym] -> Exp -> Env -> Sto -> RefVal
procedure syms expr env sto = extendSto sto $ VClosure syms expr env
