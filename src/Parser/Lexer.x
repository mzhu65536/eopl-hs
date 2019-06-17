{
{-# LANGUAGE FlexibleContexts #-}

module Parser.Lexer (
  Token(..),
  scanTokens
) where

import Control.Monad.Except

}

%wrapper "basic"

$digit = 0-9
$alpha = [a-zA-Z]
$eol   = [\n]

tokens :-

  -- Whitespace insensitive
  $eol                          ;
  $white+                       ;

  -- Comments
  "#".*                         ;

  -- Syntax
  "begin"                       { \_ -> TokenBegin     }
  "end"                         { \_ -> TokenEnd    }
  "set"                         { \_ -> TokenSet    } 
  "if"                          { \_ -> TokenIf }
  "zero?"                       { \_ -> TokenZeroP }      
  "else"                        { \_ -> TokenElse }
  "then"                        { \_ -> TokenThen }
  let                           { \_ -> TokenLet }
  rec                           { \_ -> TokenRec }
  True                          { \_ -> TokenTrue }
  False                         { \_ -> TokenFalse }
  in                            { \_ -> TokenIn }
  div                           { \_ -> TokenDiv  }

  "->"                          { \_ -> TokenArrow }
  \=                            { \_ -> TokenEq }
  \\                            { \_ -> TokenLambda }
  [\-]                          { \_ -> TokenSub }
  [\*]                          { \_ -> TokenMul }
  "$"                           { \_ -> TokenStrict }
  [\+]                          { \_ -> TokenAdd }  
  \(                            { \_ -> TokenLParen }
  \)                            { \_ -> TokenRParen }
  "{"                           { \_ -> TokenLBrace }
  "}"                           { \_ -> TokenRBrace }
  ","                           { \_ -> TokenCSep }
  ";"                           { \_ -> TokenSCep }
  ["]                           { \_ -> TokenDQuote }     
  -- List Extension
  "cons"                        { \_ -> TokenCons }
  "car"                         { \_ -> TokenCar  }
  "cdr"                         { \_ -> TokenCdr  }
  "null"                        { \_ -> TokenNil  }
  "null?"                       { \_ -> TokenNilP }
  "list"                        { \_ -> TokenList }

  -- Statement Extension
  "print"                       { \_ -> TokenPrint }
  "while"                       { \_ -> TokenWhile }
  "var"                         { \_ -> TokenVar   }

  -- Programmable Exceptions
  "try"                         { \_ -> TokenTry   }
  "raise"                       { \_ -> TokenRaise }
  "catch"                       { \_ -> TokenCatch }  

  $alpha [$alpha $digit \_ \']* { \s -> TokenSym s }
  $digit+                       { \s -> TokenNum (read s) }     
  
{
data Token 
  = TokenLet
  | TokenAdd
  | TokenSet
  | TokenDeclare
  | TokenCatch
  | TokenTry
  | TokenRaise
  | TokenPrint
  | TokenWhile
  | TokenSCep
  | TokenStrict
  | TokenBegin    
  | TokenEnd
  | TokenRec
  | TokenVar
  | TokenIf
  | TokenCSep
  | TokenSSep
  | TokenZeroP
  | TokenElse
  | TokenThen
  | TokenTrue
  | TokenFalse
  | TokenIn
  | TokenLambda
  | TokenNum Int
  | TokenSym String
  | TokenArrow
  | TokenEq
  | TokenSub
  | TokenMul
  | TokenLParen
  | TokenRParen
  | TokenLBrace
  | TokenRBrace  
  | TokenEOF
  | TokenDiv
  | TokenDQuote
  | TokenCons
  | TokenCar 
  | TokenCdr 
  | TokenNil 
  | TokenNilP
  | TokenList
  deriving (Eq,Show)
  
scanTokens :: String -> Except String [Token]
scanTokens str = go ('\n',[],str) where 
  go inp@(_,_bs,str') =
    case alexScan inp 0 of
     AlexEOF -> return []
     AlexError _ -> throwError "Invalid lexeme."
     AlexSkip  inp' len     -> go inp'
     AlexToken inp' len act -> do
      res <- go inp'
      let rest = act (take len str')
      return (rest : res)
}
