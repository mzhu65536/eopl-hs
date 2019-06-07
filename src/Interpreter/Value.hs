module Interpreter.Value where

import Interpreter.Data 
import Parser.Syntax

procedure :: Sym -> Exp -> Env -> Val
procedure sym expr env = VClosure sym expr env
