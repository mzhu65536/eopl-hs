module Main where


import Parser.Syntax
import Parser.Lexer
import Parser.Parser

import qualified Parser.Spec
import qualified Interpreter.Spec
import qualified Typed.Spec
-- import Data.Either

import Test.Tasty
import Test.Tasty.HUnit 
import Test.Tasty.Hspec

main :: IO ()
main = hspec $ do
  parallel $ do
    describe "Parser" Parser.Spec.spec
    describe "Interpreter" Interpreter.Spec.spec
    describe "Typed" Typed.Spec.spec

-- tests :: TestTree
-- tests = testGroup "TestLiteral"
--   [ testCase "parse 1 " $
--     (parseExpr "1")  @?= Right (Lit (LInt 1))
--   , testCase "parse 2 " $
--     (parseExpr "2")  @?= Right (Lit (LInt 1))
--   ]
