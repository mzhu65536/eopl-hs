module Interpreter.Storage where

import Interpreter.Data

deRef :: Sto -> Ref -> Val
deRef [] r = reportRefOutBound r
deRef (x : xs) r | r == 0 = x
                 | otherwise = deRef xs (r - 1)

extendSto :: Sto -> Val -> (Sto, Ref)
extendSto sto val = (sto ++ [val], length sto)

reportRefOutBound :: Ref -> Val
reportRefOutBound ref = VException $ "Unable to retrive ref #"++ (show ref)
