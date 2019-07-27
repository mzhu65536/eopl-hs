{-# LANGUAGE LambdaCase #-}
module Type.Type where

{-
EOPL Language Type Extension
-}

{-
Design Recipe:
1. Primitive Types -- String, Int, Curry, Bool
-}

--import Direct.Interpreter.Data

-- import Control.Monad.IO
import Control.Monad.Except
import Control.Monad.Reader

import General.Environment
import General.Eval
import Parser.Typed.Syntax

data Type = TInt
          | TBool
          | TString
          | TCurry Type Type
          | TStub
          | TVoid
          | TPair Type Type
          | TEmpty
          | TList Type
          | TRef Type -- Reference to position of given type
  deriving (Eq)


instance Show Type where
  show TInt = "int"
  show TBool = "bool"
  show TString = "string"
  show (TCurry l r) = show l ++ " -> " ++ show r
  show (TPair l r) = "(" ++ show l ++ ", " ++ show r ++ ")"
  show TEmpty = "EMPTY"
  show TStub = "<Any Type>"
  show TVoid = "VOID"
  show (TList t) = "[" ++ show t ++ "]"
  show (TRef t) = "Reference Type: " ++ show t

type AST = Exp
type Env' = Env AST String Type
type Eval' a = Eval Env' String a
type Final = IO (Either String Type)

runEval' :: AST -> Final
runEval' ast = runEval (typeCheck ast) emptyEnv

typeCheck :: AST -> Eval' Type
typeCheck (Lit l) = return $ case l of
                               LInt  _ -> TInt
                               LBool _ -> TBool
                               LStr  _ -> TString
typeCheck (Var s) = do env <- ask
                       applyEnv env s
typeCheck (ZeroP e) = do tz <- typeCheck e
                         case tz of
                           TInt -> return TBool
                           _    -> reportType TBool tz [e]
typeCheck (Let symExpVs expB) =
  do neoEnv <- extendTEnv typeCheck symExpVs
     local (appendEnv neoEnv) $ typeCheck expB
typeCheck (If expP expT expF) =
  do tPred <- typeCheck expP
     case tPred of
       TBool -> do tExpT <- typeCheck expT
                   tExpF <- typeCheck expF
                   if tExpT == tExpF
                     then return tExpT
                     else reportType tExpT tExpF [expT, expF]
       _     -> reportType TBool tPred [expP]
typeCheck (App expF expA) =
  do tFun <- typeCheck expF
     tApp <- typeCheck expA
     case tFun of
       TCurry tPrm tRet
         | tPrm == tApp -> return tRet
         | otherwise    -> reportType tPrm tApp [expA]
       _                -> reportType (TCurry tApp TStub) tFun [expF]
typeCheck (Lam tSymVs expB) =
  do neoEnv <- extendTEnv typeCheckTrivial tSymVs
     tBody <- local (appendEnv neoEnv) $ typeCheck expB
     unCurry neoEnv tBody
typeCheck (Rec funs expB) = do
  let tSymPs = map (\(x, _, _) -> x) funs
  procEnv <- extendTEnv typeCheckTrivial tSymPs
  lastEnv <- ask
  let neoEnv = procEnv `appendEnv` lastEnv
  mapM_ (\((_, lTyp), tSymVs, expF) ->
           do varEnv <- extendTEnv typeCheckTrivial tSymVs
              typF' <- typeCheckTrivial lTyp
              local (const (varEnv `appendEnv` neoEnv)) $
                do typR <- typeCheck expF
                   typF <- unCurry varEnv typR
                   if typF == typF'
                     then return typF
                     else reportType typF' typF [expF])
    funs
  local (const neoEnv) $ typeCheck expB
typeCheck (Set symV expV) = do
  lastEnv <- ask
  typV <- applyEnv lastEnv symV
  typV' <- typeCheck expV
  if typV == typV'
    then return typV
    else reportType typV typV' [expV]

typeCheck (Op opr e1 e2) = do
  t1 <- typeCheck e1
  t2 <- typeCheck e2
  case (opr, t1, t2) of
    (Diff, TInt, TInt) -> return TInt
    (Mult, TInt, TInt) -> return TInt
    (Plus, TInt, TInt) -> return TInt
    (Div, TInt, TInt)  -> return TInt
    _                  -> reportType TInt TStub [e1, e2]
typeCheck (LPr (LPCons expL expR)) = do
  t1 <- typeCheck expL
  t2 <- typeCheck expR
  return $! TPair t1 t2
typeCheck (LPr (LPDStr symL symR expP expB)) = do
  typP <- typeCheck expP
  case typP of 
    TPair typL typR -> do
      pairEnv <- extendTEnv return [(symL, typL), (symR, typR)]
      local (appendEnv pairEnv) $ typeCheck expB
    _ -> reportType (TPair TStub TStub) typP [expP]
typeCheck (Lst (Lsts expLs)) =
  case expLs of
    x : ys -> do typInit <- typeCheck x
                 checkList' ys typInit
    _      -> return TEmpty
  where
    checkList' :: [Exp] -> Type -> Eval' Type
    checkList' (x : ys) t = do
      typX <- typeCheck x
      if typX == t
        then checkList' ys t
        else reportType t typX [x]
    checkList' [] t = return $ TList t
typeCheck (Lst (Cons expX expYs)) = do
  typX <- typeCheck expX
  typYs <- typeCheck expYs
  case typYs of
    TList typY
      | typX == typY -> return typYs
      | otherwise    -> reportType typX typY [expX]
    _                -> reportType (TList typX) typYs [expYs]
typeCheck (Lst (Car expL)) = 
  typeCheck expL >>=
  \case
    TList typX -> return typX
    typQ       -> reportType (TList TStub) typQ [expL]

typeCheck (Lst (Cdr expL)) =
  typeCheck expL >>=
  \case
    typL@(TList _) -> return typL
    typQ           -> reportType (TList TStub) typQ [expL]
    
typeCheck (Lst (Nil lTypN)) = TList <$> typeCheckTrivial lTypN 
typeCheck (Lst (NilP expL)) =
  typeCheck expL >>=
  \case
    TEmpty  -> return TBool
    TList _ -> return TBool
    err     -> reportType err (TList TStub) [expL]
typeCheck (Begin expBs) = 
  checkBegin expBs where
  checkBegin [] = reportType TVoid TStub expBs
  checkBegin [expB] = typeCheck expB
  checkBegin (expV : expBs') = typeCheck expV >> checkBegin expBs'
typeCheck (ERef (ERNew expR)) =
  TRef <$> typeCheck expR
typeCheck (ERef (ERSet symV expR)) = do
  lastEnv <- ask
  applyEnv lastEnv symV >>=
    \case
      (TRef typI) ->
        do typR <- typeCheck expR
           if typI == typR
             then return TVoid
             else reportType typI typR [expR]
      typQ        -> reportType (TRef TStub) typQ [Var symV]
typeCheck (ERef (ERDer expV)) = 
  typeCheck expV >>=
    \case
      TRef typI -> return typI
      typQ     -> reportType (TRef TStub) typQ [expV]

typeCheck (MPar (MPNew expL expR)) = do
  typL <- typeCheck expL
  typR <- typeCheck expR
  return (TPair (TRef typL) (TRef typR))

typeCheck (MPar (MPLeft expP)) = do
  typP <- typeCheck expP
  case typP of
    TPair (TRef typL) (TRef _) ->
      return typL
    _                          ->
      reportType (TPair (TRef TStub) (TRef TStub)) typP [expP]

typeCheck (MPar (MPRight expP)) = do
  typP <- typeCheck expP
  case typP of
    TPair (TRef _) (TRef typR) ->
      return typR
    _                          ->
      reportType (TPair (TRef TStub) (TRef TStub)) typP [expP]

typeCheck (MPar (MPSetL expP expL)) = do
  typP <- typeCheck expP
  case typP of
    TPair (TRef typL) (TRef _) ->
      typeCheck expL >>=
      \typL' -> if typL == typL'
                then return TVoid
                else reportType typL typL' [expL]
    _                          -> 
      reportType (TPair (TRef TStub) (TRef TStub)) typP [expP]
      
typeCheck (MPar (MPSetL expP expR)) = do
  typP <- typeCheck expP
  case typP of
    TPair (TRef _) (TRef typR) ->
      typeCheck expR >>=
      \typR' -> if typR == typR'
                then return TVoid
                else reportType typR typR' [expR]
    _                          -> 
      reportType (TPair (TRef TStub) (TRef TStub)) typP [expP]  
      
      
unCurry :: Env' -> Type -> Eval' Type
unCurry lEnv result =
  return $
  foldr
  (\x ys -> case x of
              CNEnv _ t -> TCurry t ys
              CREnv{}   -> TCurry TStub ys)
  result
  (unListEnv lEnv)
    


extendTEnv :: (a -> Eval' Type) -> [(String, a)] -> Eval' Env'
extendTEnv f symAs =
  -- Use mapM to Mapping [(String, a)] into [compoundEnv]
  do ans <- mapM (\(symV, aT) -> CNEnv symV <$> f aT) symAs
     return $ ListEnv ans

typeCheckTrivial :: LType -> Eval' Type
typeCheckTrivial = return . typeCheckTrivial'
  where
    typeCheckTrivial' =
      \case
        LTInt       -> TInt
        LTBool      -> TBool
        LTString    -> TString
        LTCurry l r -> TCurry
                       (typeCheckTrivial' l)
                       (typeCheckTrivial' r)
        LTPair l r  -> TPair
                       (typeCheckTrivial' l)
                       (typeCheckTrivial' r)
        LTEmpty     -> TEmpty
        LTList l    -> TList (typeCheckTrivial' l)


reportType :: Type -> Type -> [AST] -> Eval' Type
reportType expect given at =
  throwError $!
  "Type Mismatch: \n" ++ 
  "Expect: " ++ show expect ++ "\n" ++
  "Given: " ++ show given ++ "\n" ++
  "At Expression(s): " ++ foldl (\ys x -> show x ++ "; " ++ ys) "" at

