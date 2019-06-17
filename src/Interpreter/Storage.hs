module Interpreter.Storage where

import Interpreter.Data

emptySto :: Sto
emptySto = []

updateSto :: Sto -> Ref -> Val -> RefVal
updateSto sto ref val = case updateSto' sto ref of
                          Nothing   -> extendSto sto $ reportRefOutBound ref
                          Just sto' -> (sto', ref)
    where
      updateSto' :: Sto -> Ref -> Maybe Sto
      updateSto' [] _          = Nothing
      updateSto' (x : xs) ref' =
        if ref' == 0
        then Just $ val : xs
        else updateSto' xs (ref' - 1) >>= \sto'' -> Just $  x : sto''

deRef :: Sto -> Ref -> Val
deRef [] r = reportRefOutBound r
deRef (x : xs) r | r == 0 = x
                 | otherwise = deRef xs (r - 1)

extendSto :: Sto -> Val -> (Sto, Ref)
extendSto sto val = (sto ++ [val], length sto)

reportRefOutBound :: Ref -> Val
reportRefOutBound ref = VException $
                        VStr $ "Unable to retrive ref #" ++ show ref
