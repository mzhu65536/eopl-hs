module General.Environment where

-- import qualified Data.Map as Map


import Control.Monad.Except
import General.Eval

-- Todo: Deriving ListEnv as Traversable so as to modify maps

newtype Env expType symType valType =
  ListEnv { unListEnv :: [CompondEnv symType valType expType]
          }
--            | MapEnv (Map.Map String a)

data CompondEnv s v e = CNEnv s v
                      | CREnv { procNames :: [s]
                              , procVars :: [[s]]
                              , procBodies :: [e]
                              }

appendEnv :: Env e s v -> Env e s v -> Env e s v
appendEnv (ListEnv l) (ListEnv r) = ListEnv $ l ++ r

emptyEnv :: Env e s v
emptyEnv = ListEnv []

extendNEnv :: Env e s v -> s -> v -> Env e s v
extendNEnv (ListEnv ce) s v = ListEnv $! CNEnv s v : ce

applyEnv :: Env exp String v -> String -> Eval e String v
applyEnv (ListEnv comEnv) searchVar =
  case comEnv of
    []                  -> throwError $! "No such a binding: " ++ searchVar
    CNEnv sym val : rst -> if sym == searchVar
                           then return val
                           else applyEnv (ListEnv rst) searchVar
    _                   -> throwError $! "No such a binding: " ++ searchVar
