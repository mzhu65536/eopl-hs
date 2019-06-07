-- Parser and Lexer are painful. We will implement them when doing dragon book.
-- For now, we will use happy and alex

{-

-}

module Parser.Syntax
  ( Sym
  , Exp(..)
  , Lit(..)
  , Binop(..)
  ) where

type Sym = String

-- For the pedagogical purpose, we follow datatype from EOPL 
data Exp = Lit Lit
         | Let Sym Exp Exp
         | Rec Sym Sym Exp Exp
         | Var Sym
         | Op Binop Exp Exp
         | ZeroP Exp
         | If Exp Exp Exp
         | Lam Sym Exp
         | App Exp Exp
  deriving (Show, Eq)
            
data Lit = LInt Int
         | LBool Bool
  deriving (Show, Eq)
  

data Binop = Diff | Mult | Plus
  deriving (Show, Eq)
