-- Parser and Lexer are painful. We will implement them when doing dragon book.
-- For now, we will use happy and alex

{-

-}

module Parser.Syntax
  ( Sym
  , Exp(..)
  , Lit(..)
  , Binop(..)
  , Lst(..)
  , Stm(..)
  ) where

type Sym = String

data Stm = Assign Sym Exp
         | Print Exp
         | Block [Stm]
         | IfS Exp Stm Stm
         | While Exp Stm
         | Declare [Sym] Stm

-- For the pedagogical purpose, we follow datatype from EOPL 
data Exp = Lit Lit
         | Let [(Sym, Exp)] Exp
         | Rec Sym Sym Exp Exp
         | Var Sym
         | Op Binop Exp Exp
         | ZeroP Exp
         | If Exp Exp Exp
         | Lam [Sym] Exp
         | App Exp Exp
         | Lst Lst
         | Set Sym Exp
         | Begin [Exp] 
  deriving (Show, Eq)
            
data Lit = LInt Int
         | LBool Bool
  deriving (Show, Eq)

-- List extension (Exercise 5.5 5.6)
data Lst = Cons Exp Exp
         | Nil
         | Car Exp
         | Cdr Exp
         | NilP Exp
         | Lsts [Exp]
  deriving (Show, Eq)

data Binop = Diff | Mult | Plus
  deriving (Show, Eq)

