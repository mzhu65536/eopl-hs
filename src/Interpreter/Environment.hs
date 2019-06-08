module Interpreter.Environment where

import Interpreter.Data
import Parser.Syntax
import Data.Set(empty
               ,insert
               )

emptyEnv :: Env
emptyEnv = []

extendEnv :: Env -> Sym -> Val -> Env
extendEnv [] s v = [(s, v)]
extendEnv ((s', v') : xs) s v
  | s' == s = (s, v) : xs
  | otherwise = (s', v') : extendEnv xs s v

extendEnvR :: Env -> Sym -> Sym -> Exp -> Env
extendEnvR env nameP varB expP =
  let vClosure = VClosure [varB] expP envR
      envR = extendEnv env nameP vClosure in
    envR
    

-- Use lazy evaluation so that the thunk will not evaluated if found
applyEnv :: Env -> Sym -> Val
applyEnv env s = foldr
                 (\(s', v') ys -> if s' == s then v' else ys)
                 (VException $ "No such a binding: " ++ s )
                 env

{-
Compute free variables in the given expression.
-}
-- freeVariables :: Alp -> Exp -> Alp
-- freeVariables = loop empty where
--   loop frees exp bounds = empty
                        
