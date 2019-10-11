module Direct.Interpreter.Value where

import Direct.Interpreter.Data
import Direct.Interpreter.Storage
import Parser.Syntax

procedure :: [Sym] -> Exp -> Env -> Sto -> RefVal
procedure syms expr env sto = extendSto sto $ VClosure syms expr env
