{-
Data representation 
-}

module Interpreter.Data ( Cont(..)
                        , Env
                        , Alp
                        , Val(..)
                        , Sto
                        , Ref
                        , RefVal
                        ) where
import Parser.Syntax
import Data.Set
import Control.Monad

newtype Interp 

-- type Sym = String
type Env = [(Sym, Ref)]
type Alp = Set Sym

data Val = VInt Int
         | VBool Bool
         | VException String
         | VClosure [Sym] Exp Env
         | VCons Ref Ref
         | VNil
  deriving (Eq)

type Ref = Int
type Sto = [Val]
type RefVal = (Sto, Ref)

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
          | KApp Cont Ref
          | KBiOpL Cont Binop Exp Env
          | KBiOpR Cont Binop Ref
          | KConsL Cont Exp Env
          | KConsR Cont Ref
          | KCar Cont
          | KCdr Cont
          | KNilP Cont
          | KLst Cont [Exp] Env
          | KLstCons Cont Ref
          | KSet Cont Ref
          | KBegin Cont [Exp] Env
  deriving (Show, Eq)

-- TODO: Phantom type for VNil as VEmpty to be more ``Racketionic``
