module Interpreter.Storage where

import Interpreter.Data

import Control.Monad.Writer
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Except

emptySto :: Sto
emptySto = []

updateSto :: Sto -> Ref -> Val -> RefVal
updateSto sto ref val = case updateSto' sto ref of
                          Nothing   -> extendSto sto $ reportRefOutBound ref
                          Just sto' -> (ref, sto')
    where
      updateSto' :: Sto -> Ref -> Maybe Sto
      updateSto' [] _          = Nothing
      updateSto' (x : xs) ref' =
        if ref' == 0
        then Just $ val : xs
        else (updateSto' xs (ref' - 1)) >>= \sto'' -> Just $  x : sto''

deRef :: Sto -> Ref -> Val
deRef [] r = reportRefOutBound r
deRef (x : xs) r | r == 0 = x
                 | otherwise = deRef xs (r - 1)

extendSto :: Val -> Eval Ref
extendSto val = do sto <- get
                   let (ref, sto') = (length sto, sto ++ [val])
                   put sto'
                   return ref
                   
reportRefOutBound :: Ref -> Val
reportRefOutBound ref = VException $ "Unable to retrive ref #"++ (show ref)
