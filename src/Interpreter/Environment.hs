module Interpreter.Environment where

import Interpreter.Data
import Interpreter.Storage
import Parser.Syntax

import Control.Monad.Writer
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Except

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
      ref = extendSto vClosure
      envR = extendEnv env nameP refClosure in
    (envR, sto')

extendEnvR' :: Sym -> Sym -> Exp -> Eval Ref
extendEnvR' nameP varB expP =
  do env <- ask
     let vClosure = VClosure [varB] expP envR
         ref = extendSto vClosure
         envR = extendEnv env nameP ref 
     (envR, sto')    

-- Use lazy evaluation so that the thunk will not evaluated if found
applyEnv :: Sym -> Eval Ref
applyEnv s = do env <- ask
                maybe
                  (throwError $! "no such a binding: " ++ s)
                  return $
                  foldr (\(s', r') ys -> if s' == s then Just r' else ys)
                  Nothing env


{-
Compute free variables in the given expression.
-}
-- freeVariables :: Alp -> Exp -> Alp
-- freeVariables = loop empty where
--   loop frees exp bounds = empty
                        
