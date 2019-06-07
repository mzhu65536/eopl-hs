module Interpreter.Eval where

import Interpreter.Data
import Interpreter.Value
import Interpreter.Environment
import Parser.Syntax

-- The Continuation-Passing Style Evaluation

-- Principles for Control Context Growth
---- evaluation of operands
---- non-tail recursive calls
--------------------------------------------------------------------------------
eval :: Exp -> Val
eval expr  = evalK expr emptyEnv KEmpty 

evalK :: Exp -> Env -> Cont -> Val
evalK expr env cont = case expr of
  Lit lit                 -> applyCont cont $ evalLit lit
  Var sym                 -> applyCont cont $ applyEnv env sym
  Lam sym expB            -> applyCont cont $ procedure sym expB env
  Rec symP symB expP expB -> evalK expB (extendEnvR env symP symB expP) cont
  ZeroP expZ              -> evalK expZ env (KZero cont)
  Let symV expV expB      -> evalK expV env (KLet cont symV expB env)
  If expP expT expF       -> evalK expP env (KIf cont expT expF env)
  App expF expA           -> evalK expF env (KOpr cont expA env)
  Op binop expL expR      -> evalK expL env (KBiOpL cont binop expR env)
  -- _                    -> applyCont cont $ VException "Not implemented yet"
  
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
 KLet k' symV expB env -> evalK expB (extendEnv env symV v) k'
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
  


applyProcedureK :: Val -> Val -> Cont -> Val
applyProcedureK (VClosure symV expB env) v k =
  evalK expB (extendEnv env symV v) k
applyProcedureK v _ _                        =
  reportTypeMismatch v "VClosure ..."

reportTypeMismatch :: Val -> String -> Val
reportTypeMismatch vGiven sExpected =
  VException $
  "Type Mismatch:\n" ++
  "Given: " ++ (show vGiven) ++ "\n" ++ 
  "Expected: " ++ sExpected
  
