module Interpreter.Eval where

import Interpreter.Data
import Interpreter.Value
import Interpreter.Storage
import Interpreter.Environment
import Parser.Syntax

-- TODO: More Haskellish  (Typeclass Eval, Monad ...)

-- The Continuation-Passing Style Evaluation

-- Principles for Control Context Growth
---- evaluation of operands
---- non-tail recursive calls
-------------------------------------------------------------------------------



-- invoke the interpreter and transform into desire return type
eval :: Exp -> Val
eval expr =
  let (sto, ref) = tampoline $ evalK expr emptyEnv emptySto KEmpty in
    deRef sto ref


tampoline :: Bounce RefVal -> RefVal
tampoline (BVal v) = v
tampoline (BStep b) = tampoline b -- loop that doing tail calls

evalK :: Exp -> Env -> Sto -> Cont -> Bounce RefVal
evalK expr env sto cont = case expr of
  Lit lit                  -> applyCont cont $ evalLit sto lit
  Var sym                  -> applyCont cont $ applyEnv env sto sym
  Lam syms expB            -> applyCont cont $ procedure syms expB env sto
  Rec symP symB expP expB  -> let (env', sto') =
                                    extendEnvR env sto symP symB expP in
                                evalK expB env' sto' cont
  ZeroP expZ               -> evalK expZ env sto (KZero cont)
  Let symExpS expB         -> case symExpS of
                                []                 -> evalK expB env sto cont
                                (symV, expV) : rst -> evalK expV env sto
                                                      (KLet cont rst symV
                                                       env env expB)
  If expP expT expF        -> evalK expP env sto (KIf cont expT expF env)
  App expF expA            -> evalK expF env sto (KOpr cont expA env)
  Op binop expL expR       -> evalK expL env sto (KBiOpL cont binop expR env)
  Lst (Cons expL expR)     -> evalK expL env sto (KConsL cont expR env)
  Lst Nil                  -> applyCont cont $ extendSto sto VNil
  Lst (Car expC)           -> evalK expC env sto (KCar cont)
  Lst (Cdr expC)           -> evalK expC env sto (KCdr cont)
  Lst (NilP expN)          -> evalK expN env sto (KNilP cont)
  Lst (Lsts (expF : expS)) -> evalK expF env sto (KLst cont expS env)
  Lst (Lsts [])            -> applyCont cont $ extendSto sto VNil
  Set symV expS            ->
    let (sto', r') = applyEnv env sto symV in
      case deRef sto' r' of
        VException _ -> applyCont cont (sto', r')
        _            -> evalK expS env sto (KSet cont r')
  Begin []                 -> applyCont cont $
    extendSto sto $ VException "Invalid Begin Expression"
  Begin (x : xs)           -> evalK x env sto (KBegin cont xs env)

evalLit :: Sto -> Lit -> RefVal
evalLit sto (LInt i)  = extendSto sto $ VInt i
evalLit sto (LBool b) = extendSto sto $ VBool b


applyCont :: Cont -> RefVal -> Bounce RefVal
applyCont k (sto, ref) =
  applyCont' (deRef sto ref)
  where
    applyCont' :: Val -> Bounce RefVal
    applyCont' (VException _) = BVal (sto, ref)
    applyCont' v = case k of
      KEmpty                -> BVal (sto, ref)
      KZero k'              ->
        applyCont k'
        (case v of
            VInt i -> extendSto sto (VBool (i == 0))
            _      -> extendSto sto $
                      reportTypeMismatch v "VInt ...")
      KLet k' se sV e e' eb -> -- k' sym+exps symV envOrg envAcc expBody
        let (sto', refV) = extendSto sto v
            env' = extendEnv e' sV refV in
          (case se of
             []               -> evalK eb env' sto' k'
             (s', exp') : rst -> evalK exp' e sto' $ KLet k' rst s' e env' eb)
      KIf k' expT expF env  ->
        case v of
          VBool True  -> evalK expT env sto k'
          VBool False -> evalK expF env sto k'
          _           -> BVal $ extendSto sto $ VException "Type Mismatch"
      KOpr k' expA env      -> evalK expA env sto (KApp k' ref)
      KApp k' refF          -> applyProcedureK refF (sto, ref) k'
      KBiOpL k' op expR env -> evalK expR env sto (KBiOpR k' op ref)
      KBiOpR k' op refL     -> applyCont k' $
        extendSto sto $
        case (op, deRef sto refL, v) of
          (Diff, VInt l, VInt r) -> VInt $ l - r
          (Plus, VInt l, VInt r) -> VInt $ l + r
          (Mult, VInt l, VInt r) -> VInt $ l * r
          _                      -> VException "Type Mismatch"
      KConsL k' expR env    -> evalK expR env sto (KConsR k' ref)
      KConsR k' refL        -> applyCont k' $ extendSto sto (VCons refL ref)
      KCar k'               -> applyCont k' $
        case v of
          VCons rL _ -> (sto, rL)
          _          -> extendSto sto $ reportTypeMismatch v "VCons"
      KCdr k'               -> applyCont k' $
        case v of
          VCons _ rR -> (sto, rR)
          _          -> extendSto sto $ reportTypeMismatch v "VCons"
      KNilP k'              -> applyCont k' $ extendSto sto $
        case v of
          VNil -> VBool True
          _    -> VBool False
      KLst k' expS env      -> evalK (Lst $ Lsts expS) env sto (KLstCons k' ref)
      KLstCons k' refL      -> applyCont k' $ extendSto sto (VCons refL ref)
      KSet k' refV          -> applyCont k' $ updateSto sto refV v
      KBegin k' exps env    ->
        case exps of
          []       -> applyCont k' (sto, ref)
          (x : xs) -> evalK x env sto $ KBegin k' xs env

applyProcedureK :: Ref -> RefVal -> Cont -> Bounce RefVal
applyProcedureK refClosure (sto, refApp) k =
  applyProcedureK' (deRef sto refClosure) where
  applyProcedureK' :: Val -> Bounce RefVal
  applyProcedureK' (VClosure symVs expB env) = BStep $
    case symVs of
      []            -> BVal $
                       extendSto sto $ VException "Should Have Been Evaluated"
      [symV]        -> evalK expB (extendEnv env symV refApp) sto k
      symV : symVs' -> applyCont k $
                       extendSto sto
                       (VClosure symVs' expB $ extendEnv env symV refApp)
  applyProcedureK' v                       = BVal $
    extendSto sto $ reportTypeMismatch v "VClosure ..."

reportTypeMismatch :: Val -> String -> Val
reportTypeMismatch vGiven sExpected =
  VException $
  "Type Mismatch:\n" ++
  "Given: " ++ show vGiven ++ "\n" ++
  "Expected: " ++ sExpected

