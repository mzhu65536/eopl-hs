module Interpreter.Environment where

import Interpreter.Data
import Interpreter.Storage
import Parser.Syntax


emptyEnv :: Env
emptyEnv = []

extendEnv :: Env -> Sym -> Ref -> Env
extendEnv [] s r = [(s, r)]
extendEnv ((s', r') : xs) s r
  | s' == s = (s, r) : xs
  | otherwise = (s', r') : extendEnv xs s r

extendEnvR :: Env -> Sto -> Sym -> Sym -> Exp -> (Env, Sto)
extendEnvR env sto nameP varB expP =
  let vClosure = VClosure [varB] expP envR
      (sto', refClosure) = extendSto sto vClosure
      envR = extendEnv env nameP refClosure in
    (envR, sto')
    

-- Use lazy evaluation so that the thunk will not evaluated if found
applyEnv :: Env -> Sto -> Sym -> RefVal
applyEnv env sto s = foldr
                     (\(s', r') ys -> if s' == s then (sto, r') else ys)
                     (extendSto sto $ VException $ "No such a binding: " ++ s )
                     env

{-
Compute free variables in the given expression.
-}
-- freeVariables :: Alp -> Exp -> Alp
-- freeVariables = loop empty where
--   loop frees exp bounds = empty
                        
