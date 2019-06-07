{
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Parser.Parser (
  parseExpr,
  parseTokens,
) where

import Parser.Lexer
import Parser.Syntax

import Control.Monad.Except

}

-- Entry point
%name expr

-- Entry point
%name expr

-- Lexer structure 
%tokentype { Token }

-- Parser monad
%monad { Except String } { (>>=) } { return }
%error { parseError }

-- Token Names
%token
    "if"    { TokenIf }
    "else"  { TokenElse }
    "then"  { TokenThen }
    "zero?" { TokenZeroP }
    let   { TokenLet }
    rec   { TokenRec }
    true  { TokenTrue }
    false { TokenFalse }
    '$'  { TokenStrict }
    in    { TokenIn }
    NUM   { TokenNum $$ }
    VAR   { TokenSym $$ }
    '\\'  { TokenLambda }
    '->'  { TokenArrow }
    '='   { TokenEq }
    '-'   { TokenSub }
    '+'   { TokenAdd }
    '*'   { TokenMul }
    '('   { TokenLParen }
    ')'   { TokenRParen }

-- Operators
%left '+' '-'
%left '*' '$'
%%

Expr : let VAR '=' Expr in Expr     { Let $2 $4 $6 }
     | rec VAR VAR '=' Expr in Expr { Rec $2 $3 $5 $7 }
     | '\\' VAR '->' Expr           { Lam $2 $4 }
     | "if" Expr "then" Expr "else" Expr  { If $2 $4 $6 }
     | "zero?" Expr                 { ZeroP $2 }
     | Form                         { $1 }

Form : Form '+' Form                { Op Plus $1 $3 }
     | Form '-' Form                { Op Diff $1 $3 }
     | Form '*' Form                { Op Mult $1 $3 }
     | Fact                         { $1 }

Fact : Fact Atom                    { App $1 $2 }
     | Fact '$' Atom                { App $1 $3 }
     | Atom                         { $1 }


Atom : '(' Expr ')'                 { $2 }
     | NUM                          { Lit (LInt $1) }
     | VAR                          { Var $1 }
     | true                         { Lit (LBool True) }
     | false                        { Lit (LBool False) }

{

parseError :: [Token] -> Except String a
parseError (l:ls) = throwError (show l)
parseError [] = throwError "Unexpected end of Input"

parseExpr :: String -> Either String Exp
parseExpr input = runExcept $ do
  tokenStream <- scanTokens input
  expr tokenStream

parseTokens :: String -> Either String [Token]
parseTokens = runExcept . scanTokens
    
}
