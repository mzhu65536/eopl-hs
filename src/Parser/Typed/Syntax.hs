-- Parser and Lexer are painful. We will implement them when doing dragon book.
-- For now, we will use happy and alex

{-

-}

module Parser.Typed.Syntax where
type Sym = String


data LType = LTInt
           | LTBool
           | LTString
           | LTCurry LType LType
           | LTPair LType LType
           | LTList LType
           | LTEmpty
  deriving (Show, Eq)


type TSym = (Sym, LType)

data Stm = Assign Sym Exp
         | Print Exp
         | Block [Stm]
         | IfS Exp Stm Stm
         | While Exp Stm
         | Declare [Sym] Stm

-- For the pedagogical purpose, we follow datatype from EOPL 
data Exp = Lit Lit
         | Let [(Sym, Exp)] Exp
         | Rec [(TSym, [TSym], Exp)] Exp
         | Var Sym
         | TVar TSym
         | Op Binop Exp Exp
         | ZeroP Exp
         | If Exp Exp Exp
         | Lam [TSym] Exp
         | App Exp Exp
         | Lst Lst
         | LPr LPr
         | Set Sym Exp
         | Begin [Exp]
         | Try Exp Sym Exp
         | Raise Exp
         | ERef ERef
         | MPar MPar
  deriving (Show, Eq)

data Lit = LInt Int
         | LBool Bool
         | LStr String
  deriving (Show, Eq)

-- List extension (Exercise 5.5 5.6)
data Lst = Cons Exp Exp
         | Nil LType
         | Car Exp
         | Cdr Exp
         | NilP Exp
         | Lsts [Exp]
  deriving (Show, Eq)

-- EXPLICIT REF Extension
data ERef = ERNew Exp
          | ERSet Sym Exp
          | ERDer Exp
  deriving (Show, Eq)

data LPr = LPCons Exp Exp
         | LPDStr Sym Sym Exp Exp
  deriving (Show, Eq)

data MPar = MPNew Exp Exp
          | MPLeft Exp
          | MPRight Exp
          | MPSetL Exp Exp
          | MPSetR Exp Exp
  deriving (Show, Eq)

data Binop = Diff | Mult | Plus | Div
  deriving (Show, Eq)

