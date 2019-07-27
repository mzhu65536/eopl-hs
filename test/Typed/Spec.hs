{-# LANGUAGE LambdaCase #-}
module Typed.Spec where

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Hspec

import Control.Monad.Except
import Parser.Typed.Parser
import Parser.Typed.Syntax
import Type.Type

spec :: Spec
spec = parallel $ do
  describe "type check primitive" $ do
    it "TInt" $
      parseTypeCheck "65536" `shouldReturn` Right TInt
    it "TBool True" $
      parseTypeCheck "True" `shouldReturn` Right TBool
    it "TBool False" $
      parseTypeCheck "False" `shouldReturn` Right TBool

  describe "type check ZeroP" $ do
    it "zero? 1" $
      parseTypeCheck "zero? 1" `shouldReturn` Right TBool
    it "zero? True" $
      shouldFailIO $ parseTypeCheck "zero? True"
  describe "type check let" $ do
    it "let single" $
      parseTypeCheck "let a = 1 in a" `shouldReturn` Right TInt
    it "let single seperate" $
      parseTypeCheck "let a = 1 in zero? a" `shouldReturn` Right TBool
    it "let multiple 1" $
      parseTypeCheck "let a = 1, b = 3, c = True in a"
      `shouldReturn` Right TInt
    it "let multiple 2" $
      parseTypeCheck "let a = 1, b = 3, c = True in c "
      `shouldReturn` Right TBool
  describe "type check if" $ do
    it "if bool then bool else bool" $
      parseTypeCheck
      ("let a = True, b = False, c = False in " ++
       "if a then b else c ")
      `shouldReturn` Right TBool
    it "if bool then int else int" $
      parseTypeCheck
      ("let a = True, b = 2, c = 3 in " ++
       "if a then b else c ")
      `shouldReturn` Right TInt
    it "if int then bool else bool" $
      shouldFailIO $ parseTypeCheck
      ("let a = 3, b = False, c = False in " ++
       "if a then b else c ")
    it "if bool then int else bool" $
      shouldFailIO $ parseTypeCheck
      ("let a = True, b = 1, c = False in " ++
       "if a then b else c ")
    it "if does not check branch if predicate is not bool" $
      parseTypeCheck "if 1 then (if false then 1 else false) else 1"
      `shouldReturn`
      (Left $
       "Type Mismatch: \n" ++ 
       "Expect: " ++ (show TBool) ++ "\n" ++
       "Given: " ++ (show TInt) ++ "\n" ++
       "At Expression(s): " ++ show (Lit (LInt 1)) ++ "; ")
      
  describe "type check lambda" $ do
    it "bool -> int -> int" $
      parseTypeCheck "\\ (x : bool) (y : int) -> if x then y else y"
      `shouldReturn` (Right $ TCurry TBool (TCurry TInt TInt))
  describe "type check apply" $ do
    it "(bool -> int) bool" $
      parseTypeCheck "(\\ (x : bool) -> if x then 1 else 2) False"
      `shouldReturn` Right TInt
    it "(bool -> bool -> int) bool bool" $
      parseTypeCheck
      ("(\\ (t: bool) (x : bool) -> if x then 1 else 2) " ++
      "False False")
      `shouldReturn` Right TInt
    it "(bool -> bool -> int) Partial " $
      parseTypeCheck
      ("(\\ (t: bool) (x : bool) -> if x then 1 else 2) " ++
      "False")
      `shouldReturn` Right (TCurry TBool TInt)
    it "(bool -> bool -> int) Not a function" $
      shouldFailIO $ parseTypeCheck
      ("(\\ (t: bool) (x : bool) -> if x then 1 else 2) " ++
      "False False 2")
    it "(bool -> bool -> int) Ill-typed Applicator " $
      shouldFailIO $ parseTypeCheck
      ("(\\ (t: bool) (x : bool) -> if x then 1 else 2) " ++
       "False 2")
    it "((bool -> int) -> bool -> int) bool int" $
      parseTypeCheck
      ("(\\ (t: bool -> int) (b: bool) -> t b)" ++
       "(\\ (x: bool) -> if x then 1 else 0)" ++
       "False")
      `shouldReturn` Right TInt
  describe "type check letrec" $ do
    it "sum" $
      parseTypeCheck
      ("rec int -> int sum (n : int) = " ++
       "if zero? n then 0 else n + (sum (n - 1)) in " ++
       "sum 65536")
      `shouldReturn` Right TInt
  describe "type check set" $ do
    it "mutate int" $
      parseTypeCheck
      ("let a = 2, b = False in " ++
       "set a = 544 ")
      `shouldReturn` Right TInt
    it "mutate ill type" $
      parseTypeCheck
      ("let a = 2, b = False in " ++
       "set a = False ")    
      `shouldReturn` 
      (Left $
       "Type Mismatch: \n" ++ 
       "Expect: " ++ show TInt ++ "\n" ++
       "Given: " ++ show TBool ++ "\n" ++
       "At Expression(s): " ++ show (Lit (LBool False)) ++ "; ")
  describe "type check pair" $ do
    it "int bool pair" $
      parseTypeCheck
      ("let p1 = pair (True, 2) in " ++
       "unpair l r = p1 in if l then r else 2")    
      `shouldReturn`
      Right TInt
    it "unpair a int " $
      parseTypeCheck
      ("let p1 = 2 in " ++
       "unpair l r = p1 in if l then r else 2")    
      `shouldReturn` 
      (Left $
       "Type Mismatch: \n" ++ 
       "Expect: " ++ show (TPair TStub TStub) ++ "\n" ++
       "Given: " ++ show TInt ++ "\n" ++
       "At Expression(s): " ++ show (Var "p1") ++ "; ")
  describe "type check list" $ do
    it "listof int to int" $
      parseTypeCheck
      ("let l = [ (\\(x:int) -> x) " ++
       "        , (\\(y:int) -> y) " ++
       "        , (\\(z:int) -> z) ] in " ++
       "l")
      `shouldReturn` Right (TList (TCurry TInt TInt))
    it "listof int to int get first" $
      parseTypeCheck
      ("let l = [ (\\(x:int) -> x) " ++
       "        , (\\(y:int) -> y) " ++
       "        , (\\(z:int) -> z) ] in " ++
       "(car l)")
      `shouldReturn` Right (TCurry TInt TInt)
    it "listof int to int apply second" $
      parseTypeCheck
      ("let l = [ (\\(x:int) -> x) " ++
       "        , (\\(y:int) -> y) " ++
       "        , (\\(z:int) -> z) ] in " ++
       "(car (cdr l)) 2")
      `shouldReturn` Right TInt
    it "consof int to int" $
      parseTypeCheck
      ("let l = cons (\\(x:int) -> x) " ++
       "        cons (\\(y:int) -> y) " ++
       "        cons (\\(z:int) -> z) null : (int -> int) in " ++
       "l ")
      `shouldReturn` Right (TList (TCurry TInt TInt))
    it "consof int to int apply int" $
      parseTypeCheck
      ("let l = cons (\\(x:int) -> x) " ++
       "        cons (\\(y:int) -> y) " ++
       "        cons (\\(z:int) -> z) null : (int -> int) in " ++
       "(car l)")
      `shouldReturn` Right (TCurry TInt TInt)
    it "consof int to int apply get last" $
      parseTypeCheck
      ("let l = cons (\\(x:int) -> x) " ++
       "        cons (\\(y:int) -> y) " ++
       "        cons (\\(z:int) -> z) null : (int -> int) in " ++
       "(cdr (cdr (cdr l)))")
      `shouldReturn` Right (TList (TCurry TInt TInt))
    it "consof int to int null? last" $
      parseTypeCheck
      ("let l = cons (\\(x:int) -> x) " ++
       "        cons (\\(y:int) -> y) " ++
       "        cons (\\(z:int) -> z) null : (int -> int) in " ++
       "null? (cdr (cdr (cdr l)))")
      `shouldReturn` Right TBool      
    it "consof int to int null? int" $
      shouldFailIO $
      parseTypeCheck
      ("let l = cons (\\(x:int) -> x) " ++
       "        cons (\\(y:int) -> y) " ++
       "        cons (\\(z:int) -> z) null : (int -> int) in " ++
       "null? (car (cdr (cdr l))) 2")
  describe "type check explicit reference" $ do
    it "newref int" $
      parseTypeCheck
      ("let r1 = newref (if True then 1 else 2) in " ++
       "  r1")
      `shouldReturn`
      Right (TRef TInt)
    it "newref fail with innertype" $
      shouldFailIO $
      parseTypeCheck
      ("let r1 = newref (if 1 then 1 else 2) in " ++
       "  r1")
    it "deref int" $
      parseTypeCheck
      ("let r1 = newref (if True then 1 else 2) in " ++
       "  deref r1")
      `shouldReturn`
      Right TInt
    it "setref int" $
      parseTypeCheck
      ("let r1 = newref (if True then 1 else 2) in " ++
       " begin (setref r1 = 65536), " ++
       "  deref r1 end")
      `shouldReturn`
      Right TInt
    it "setref fails on non-reference" $
      shouldFailIO $
      parseTypeCheck
      ("let r1 = newref (if True then 1 else 2), r2 = 10 in " ++
       " begin (setref r2 = 65536), " ++
       "  deref r2 end")
      
shouldFailIO :: Final -> Expectation
shouldFailIO a =
  a >>= (`shouldSatisfy`
          (\case
              Left _ -> True
              _      -> False))


parseTypeCheck :: String -> Final
parseTypeCheck s = case parseExprTyped s of
                     Left err -> return $ Left $ "Parsing Error: " ++ err
                     Right ast -> runEval' ast

