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
                        , Bounce(..)
                        ) where
import Parser.Syntax
import Data.Set
import Control.Monad

-- type Sym = String
type Env = [(Sym, Ref)]
type Alp = Set Sym

data Val = VInt Int
         | VBool Bool
         | VException Val
         | VClosure [Sym] Exp Env
         | VCons Ref Ref
         | VStr String
         | VNil
  deriving (Eq)

type Ref = Int
type Sto = [Val]
type RefVal = (Sto, Ref)

data Bounce a = BVal RefVal
              | BStep (Bounce a)

instance Show Val where
  show (VInt i)           = "Int: " ++ show i
  show (VBool b)          = "Bool: " ++ show b
  show (VException s)     = "Exception:\n" ++ show s
  show (VClosure s e env) =
    "Closure:\n" ++
    "Variables: " ++ show s ++ "\n" ++
    "Expression: " ++ show e ++ "\n" ++
    "Env: " ++ show env
  show (VCons l r)        = "Cons: " ++ show l ++ " " ++ show r
  show VNil               = "Nil"
  show (VStr s)           = s

data Cont = KEmpty
          | KZero    Cont
          | KLet     Cont [(Sym, Exp)] Sym Env Env Exp
          | KIf      Cont Exp Exp Env
          | KOpr     Cont Exp Env
          | KApp     Cont Ref
          | KBiOpL   Cont Binop Exp Env
          | KBiOpR   Cont Binop Ref
          | KConsL   Cont Exp Env
          | KConsR   Cont Ref
          | KCar     Cont
          | KCdr     Cont
          | KNilP    Cont
          | KLst     Cont [Exp] Env
          | KLstCons Cont Ref
          | KSet     Cont Ref
          | KBegin   Cont [Exp] Env
          | KTry     Cont Sym Exp Env
          | KRaise   Cont
  deriving (Show, Eq)

-- TODO: Phantom type for VNil as VEmpty to be more ``Racketionic``
