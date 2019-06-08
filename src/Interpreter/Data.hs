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
         | VClosure [Sym] Exp Env
         | VCons Val Val
         | VNil
  deriving (Eq)

instance Show Val where
  show (VInt i)           = "Int: " ++ show i 
  show (VBool b)          = "Bool: " ++ show b
  show (VException s)     = "Exception:\n" ++ s
  show (VClosure s e env) =
    "Closure:\n" ++
    "Variables: " ++ (show s) ++ "\n" ++
    "Expression: " ++ (show e) ++ "\n" ++
    "Env: " ++ (show env)
  show (VCons l r)        = "Cons: " ++ (show l) ++ " " ++ (show r)
  show (VNil)             = "Nil"

data Cont = KEmpty 
          | KZero Cont
          | KLet Cont [(Sym, Exp)] Sym Env Env Exp
          | KIf Cont Exp Exp Env
          | KOpr Cont Exp Env
          | KApp Cont Val
          | KBiOpL Cont Binop Exp Env
          | KBiOpR Cont Binop Val
          | KConsL Cont Exp Env
          | KConsR Cont Val
          | KCar Cont
          | KCdr Cont
          | KNilP Cont
          | KLst Cont [Exp] Env
          | KLstCons Cont Val 
  deriving (Show, Eq)

-- TODO: Phantom type for VNil as VEmpty to be more ``Racketionic``
