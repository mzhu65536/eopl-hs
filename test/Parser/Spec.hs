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
      (Right (Let [(("a"), (Lit (LInt 1)))] (Lit (LInt 2))))
  describe "parsing if" $ do
    it "parsing if" $
      shouldBe
      (parseExpr "if a then b else c")
      (Right (If (Var "a") (Var "b") (Var "c")))
  describe "parsing lambda" $ do
    it "parsing lam" $
      shouldBe
      (parseExpr "\\a -> 1 - 2 - a")
      (Right (Lam ["a"]
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
               (Lam ["c"] (Op Diff (Var "c") (Var "a")))
               (Lit $ LInt 65536)))
  describe "parsing list" $ do
    it "list" $
      shouldBe
      (parseExpr "list (1, 2, 3, 4)")
      (Right (Lst $ Lsts [ Lit $ LInt 1
                         , Lit $ LInt 2
                         , Lit $ LInt 3
                         , Lit $ LInt 4 ]))
    it "cons list" $
      shouldBe
      (parseExpr "cons f list (1, 2, 3, 4)")
      (Right (Lst $ Cons
              (Var "f")
              (Lst $ Lsts [ Lit $ LInt 1
                          , Lit $ LInt 2
                          , Lit $ LInt 3
                          , Lit $ LInt 4 ])))
    it "cons pair" $
      shouldBe
      (parseExpr "cons 1 2")
      (Right (Lst $ Cons
              (Lit $ LInt 1)
              (Lit $ LInt 2)))
    it "car pair" $
      shouldBe
      (parseExpr "car cons 1 2") $
      Right $
      Lst $ Car $ Lst $ Cons (Lit $ LInt 1) (Lit $ LInt 2)
    it "car pair" $
      shouldBe
      (parseExpr "cdr cons 1 2") $
      Right $
      Lst $ Cdr $ Lst $ Cons (Lit $ LInt 1) (Lit $ LInt 2)
    it "cons null" $
      shouldBe
      (parseExpr "cons 1 null") $
      Right $
      Lst $ Cons (Lit $ LInt 1) (Lst Nil)
    it "null? null" $
      shouldBe
      (parseExpr "null? null") $
      Right $
      Lst $ NilP (Lst Nil)
  describe "parsing begin block" $ do
    it "begin mono" $
      shouldBe
      (parseExpr "begin let a = 1 in a * 2 end") $
      Right $
      Begin [Let [("a", Lit $ LInt 1)] (Op Mult (Var "a") (Lit $ LInt 2))]
  describe "parsing exception" $ do
    it "raise error" $
      shouldBe
      (parseExpr "raise \"ParsingError\" " ) $
      Right $ Raise (Lit $ LStr "ParsingError")
    it "catch simple" $
      shouldBe
      (parseExpr "try 1 + 1 catch (err) 1 + 1") $
      Right $
      Try (Op Plus (Lit $ LInt 1) (Lit $ LInt 1)) "err"
      (Op Plus (Lit $ LInt 1) (Lit $ LInt 1))
