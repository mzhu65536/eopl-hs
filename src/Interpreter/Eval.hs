module Interpreter.Eval where

import Interpreter.Data
import Interpreter.Value
import Interpreter.Environment
import Parser.Syntax

-- TODO: More Haskellish  (Typeclass Eval, Monad ...)

-- The Continuation-Passing Style Evaluation

-- Principles for Control Context Growth
---- evaluation of operands
---- non-tail recursive calls
--------------------------------------------------------------------------------
eval :: Exp -> Val
eval expr  = evalK expr emptyEnv KEmpty 

evalK :: Exp -> Env -> Cont -> Val
evalK expr env cont = case expr of
  Lit lit                  -> applyCont cont $ evalLit lit
  Var sym                  -> applyCont cont $ applyEnv env sym
  Lam syms expB            -> applyCont cont $ procedure syms expB env
  Rec symP symB expP expB  -> evalK expB (extendEnvR env symP symB expP) cont
  ZeroP expZ               -> evalK expZ env (KZero cont)
  Let symExpS expB         -> case symExpS of
                                []                 -> evalK expB env cont
                                (symV, expV) : rst -> evalK expV env
                                                      (KLet cont rst symV
                                                       env env expB)
  If expP expT expF        -> evalK expP env (KIf cont expT expF env)
  App expF expA            -> evalK expF env (KOpr cont expA env)
  Op binop expL expR       -> evalK expL env (KBiOpL cont binop expR env)
  Lst (Cons expL expR)     -> evalK expL env (KConsL cont expR env)
  Lst Nil                  -> applyCont cont VNil
  Lst (Car expC)           -> evalK expC env (KCar cont)
  Lst (Cdr expC)           -> evalK expC env (KCdr cont)
  Lst (NilP expN)          -> evalK expN env (KNilP cont)
  Lst (Lsts (expF : expS)) -> evalK expF env (KLst cont expS env)
  Lst (Lsts [])            -> applyCont cont VNil
  -- _                     -> applyCont cont $ VException "Not implemented yet"


evalLit :: Lit -> Val
evalLit (LInt i)  = VInt i
evalLit (LBool b) = VBool b


applyCont :: Cont -> Val -> Val
applyCont _ (VException s) = (VException s)
applyCont k v              = case k of
 KEmpty                -> v
 KZero k'              -> applyCont k' (case v of
                                          VInt i -> VBool $ i == 0
                                          _      -> VException "Type Mismatch")
 KLet k' se sV e e' eb -> -- k' sym+exps symV envOrg envAcc expBody
   let env' = (extendEnv e' sV v) in
     case se of
       []               -> evalK eb env' k'
       (s', exp') : rst -> evalK exp' e $ KLet k' rst s' e env' eb
 KIf k' expT expF env  -> case v of
                            VBool True  -> evalK expT env k'
                            VBool False -> evalK expF env k'
                            _  -> VException "Type Mismatch"
 KOpr k' expA env      -> evalK expA env (KApp k' v)
 KApp k' valF          -> applyProcedureK valF v k'
 KBiOpL k' op expR env -> evalK expR env (KBiOpR k' op v)
 KBiOpR k' op vL       -> applyCont k' $
  case (op, vL, v) of
    (Diff, VInt l, VInt r) -> VInt $ l - r
    (Plus, VInt l, VInt r) -> VInt $ l + r
    (Mult, VInt l, VInt r) -> VInt $ l * r
    _                      -> VException "Type Mismatch"
 KConsL k' expR env    -> evalK expR env (KConsR k' v)
 KConsR k' valL        -> applyCont k' (VCons valL v)
 KCar k'               -> applyCont k' $ case v of
                                           VCons vL _ -> vL
                                           _          -> reportTypeMismatch v
                                                         "VCons" 
 KCdr k'               -> applyCont k' $ case v of
                                           VCons _ vR -> vR
                                           _          -> reportTypeMismatch v
                                                         "VCons"
 KNilP k'              -> applyCont k' $ case v of
                                           VNil -> VBool True
                                           _    -> VBool False
 KLst k' expS env      -> evalK (Lst $ Lsts expS) env (KLstCons k' v)
 KLstCons k' valL      -> applyCont k' (VCons valL v)

applyProcedureK :: Val -> Val -> Cont -> Val
applyProcedureK (VClosure symVs expB env) v k =
  case symVs of
    []            -> VException "Should Have Been Evaluated"
    [symV]        -> evalK expB (extendEnv env symV v) k
    symV : symVs' -> applyCont k (VClosure symVs' expB $ extendEnv env symV v) 
  -- 
applyProcedureK v _ _                        =
  reportTypeMismatch v "VClosure ..."

reportTypeMismatch :: Val -> String -> Val
reportTypeMismatch vGiven sExpected =
  VException $
  "Type Mismatch:\n" ++
  "Given: " ++ (show vGiven) ++ "\n" ++ 
  "Expected: " ++ sExpected
  
