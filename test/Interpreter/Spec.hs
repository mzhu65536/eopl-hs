module Interpreter.Spec where

import Interpreter.Data
import Interpreter.Eval
import Parser.Syntax
import Parser.Parser

import Test.Tasty
import Test.Tasty.HUnit 
import Test.Tasty.Hspec

parseEval :: String -> Val
parseEval s = case parseExpr s of
  Left err -> VException err
  Right ast -> eval ast
  
  
spec :: Spec
spec = parallel $ do
  describe "eval primitive types" $ do
    it "Int" $
       shouldBe (parseEval "1") $ VInt 1
    it "Bool True" $
      shouldBe (parseEval "True") $ VBool True
    it "Bool False" $
      shouldBe (parseEval "False") $ VBool False
  describe "eval if" $ do
    it "if true" $
      shouldBe
      (parseEval $
       "if (zero? (2 - 2)) " ++
       "then 10 - 2" ++ 
       "else 4 * 4")
      $ VInt 8 
    it "if false" $
      shouldBe
      (parseEval $
       "if (zero? (2 - 1)) " ++
       "then 10 - 2" ++ 
       "else 4 * 4")
      $ VInt 16        
    it "if false" $
      shouldBe
      (parseEval $
       "if (zero? (2 - 1)) " ++
       "then 10 - 2" ++ 
       "else 4 * 4")
      $ VInt 16      
  describe "eval let" $ do  
    it "let simple" $
      shouldBe
      (parseEval $
       "let x = 10 in x + 2"
      )
      $ VInt 12
    it "let shadow" $
      shouldBe
      (parseEval $
       "let x = 10 in " ++
       "let x = 11 in " ++
       "let x = 12 in " ++
       "let x = 13 in x + 2"
      )
      $ VInt 15
    it "let nested" $
      shouldBe
      (parseEval $
       "let x = 10 in " ++
       "let y = 11 in " ++
       "let z = 12 in " ++
       "let k = 13 in x + y + z + k"
      )
      $ VInt 46
    it "let operator position" $
      shouldBe
      (parseEval $
      "let x = (let y = 2 in y) in x")
      $ VInt 2
    it "let pollution" $
      shouldBe
      (parseEval $
      "let x = (let y = 2 in y) in y")
      $ VException "No such a binding: y"
    it "let multiple arguments" $
      shouldBe 
      (parseEval $
       "let x = 2 , " ++
       "    y = 5 , " ++
       "    z = 6 , " ++
       "    k = 7 in " ++
       " x + y + z + k ")
      $ VInt 20
  describe "eval rec" $ do        
    it "rec fact" $
      shouldBe
      (parseEval $
      "rec fact x = if zero? x then 1 else x * (fact (x - 1)) in " ++
      "(fact 10)")
      $ VInt 3628800
  describe "eval lambda" $ do
    it "y combinator " $
      shouldBe
      (parseEval $
      "\\b -> ((\\f -> b (\\x -> (f f) x))(\\f -> b (\\x -> (f f) x)))"
      )
      $
      VClosure
      ["b"]
      (App
       (Lam ["f"]
        (App
         (Var "b")
         (Lam ["x"] (App (App (Var "f") (Var "f")) (Var "x")))))
       (Lam ["f"]
        (App
         (Var "b")
         (Lam ["x"] (App (App (Var "f") (Var "f")) (Var "x")))))) []

    it "fact functor" $
      shouldBe
      (parseEval $
       "\\f -> if zero? x then 1 else x * (f (x - 1))")
      (VClosure ["f"]
         (If
          (ZeroP (Var "x"))
          (Lit (LInt 1))
          (Op Mult
            (Var "x")
            (App (Var "f") (Op Diff (Var "x") (Lit (LInt 1))))))
         [])
    it "apply first class" $
      shouldBe
      (parseEval $
      "(\\f -> \\x -> f x) (\\x -> x + 1) 1")
      $ VInt 2
    it "apply functor" $
      shouldBe
      (parseEval $
       "let Y = " ++
       "\\f -> (\\g -> g g) (\\g -> f (\\a -> (g g) a)) " ++
       "in " ++
       "let fact = \\f -> \\x -> if zero? x then 1 else x * (f (x - 1)) in " ++
       "(Y fact) 3") 
      $ VInt 6
    it "apply lambda 3" $
      shouldBe
      (parseEval $
      "(\\x y z ->car cdr (list (x, y, z))) 1 2 3"
      )
      $ VInt 2

    it "apply partial" $
      shouldBe
      (parseEval $
       "let a = \\f x y -> f (x + y) in " ++
       "  let b = (a (\\x -> x + 1)) in  " ++
       "    let c = b 2 in " ++
       "       c 2"
      )
      $ VInt 5
  describe "eval apply" $ do
    it "Apply" $
      shouldSatisfy
      (parseEval $
      "let a = 1 in " ++
      "let b = 2 in a b")
      $ isVException
  describe "eval list" $ do
    it "hybrid test" $
      shouldBe
      (parseEval $
      "car cdr car list ((list (1, 2, 3)), 4, 5, 6)")
      (VInt 2)
      

      

isVException :: Val -> Bool
isVException (VException _) = True
isVException _              = False
