module Interpreter.Value where

import Interpreter.Data 
import Parser.Syntax

procedure :: [Sym] -> Exp -> Env -> Val
procedure syms expr env = VClosure syms expr env
