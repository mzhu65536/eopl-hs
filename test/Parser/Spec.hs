module Parser.Spec where

import Parser.Syntax
import Parser.Lexer
import Parser.Parser
-- import Data.Either

import Test.Tasty
import Test.Tasty.HUnit 
import Test.Tasty.Hspec


spec :: Spec
spec = parallel $ do
  describe "parsing literal" $ do
    it "1 is LInt 1" $
      shouldBe (parseExpr "1") (Right (Lit (LInt 1)))
    it "1 is not LInt 2" $
      shouldNotBe (parseExpr "1") (Right (Lit (LInt 2)))
  describe "parsing let" $ do
    it "parsing let" $
      shouldBe
      (parseExpr "let a = 1 in 2")
      (Right (Let ("a") (Lit (LInt 1)) (Lit (LInt 2))))
  describe "parsing if" $ do
    it "parsing if" $
      shouldBe
      (parseExpr "if a then b else c")
      (Right (If (Var "a") (Var "b") (Var "c")))
  describe "parsing lambda" $ do
    it "parsing lam" $
      shouldBe
      (parseExpr "\\a -> 1 - 2 - a")
      (Right (Lam "a"
               (Op Diff
                 (Op Diff (Lit $ LInt 1) (Lit $ LInt 2))
                 (Var "a"))))
  describe "parsing zero-p"  $ do
    it "zero? 65536" $
      shouldBe
      (parseExpr "zero? 65536")
      (Right (ZeroP (Lit $ LInt 65536)))
  describe "parsing application " $ do
    it "apply lambda" $
      shouldBe
      (parseExpr "(\\c -> c - a)  65536")
      (Right (App
               (Lam "c" (Op Diff (Var "c") (Var "a")))
               (Lit $ LInt 65536)))             

  
