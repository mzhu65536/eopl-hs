{-
Data representation 
-}

module Interpreter.Data ( Cont(..)
                        , Env
                        , Alp
                        , Val(..)
                        ) where
import Parser.Syntax
import Data.Set

-- type Sym = String
type Env = [(Sym, Val)]
type Alp = Set Sym

data Val = VInt Int
         | VBool Bool
         | VException String
         | VClosure Sym Exp Env
  deriving (Eq)

instance Show Val where
  show (VInt i)           = "Int: " ++ show i 
  show (VBool b)          = "Bool: " ++ show b
  show (VException s)     = "Exception:\n" ++ s
  show (VClosure s e env) = "Closure:\n" ++ (show s) ++ (show e) ++
                            "\n" ++ (show env)
 
  
  

data Cont = KEmpty 
          | KZero Cont
          | KLet Cont Sym Exp Env
          | KIf Cont Exp Exp Env
          | KOpr Cont Exp Env
          | KApp Cont Val
          | KBiOpL Cont Binop Exp Env
          | KBiOpR Cont Binop Val
  deriving (Show, Eq)

