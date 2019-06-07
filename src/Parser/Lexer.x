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
  "if"                          { \_ -> TokenIf }
  "zero?"                       { \_ -> TokenZeroP }      
  "else"                        { \_ -> TokenElse }
  "then"                        { \_ -> TokenThen }
  let                           { \_ -> TokenLet }
  rec                           { \_ -> TokenRec }
  True                          { \_ -> TokenTrue }
  False                         { \_ -> TokenFalse }
  in                            { \_ -> TokenIn }
  $digit+                       { \s -> TokenNum (read s) }
  "->"                          { \_ -> TokenArrow }
  \=                            { \_ -> TokenEq }
  \\                            { \_ -> TokenLambda }
  [\-]                          { \_ -> TokenSub }
  [\*]                          { \_ -> TokenMul }
  "$"                          { \_ -> TokenStrict }
  [\+]                          { \_ -> TokenAdd }  
  \(                            { \_ -> TokenLParen }
  \)                            { \_ -> TokenRParen }
  $alpha [$alpha $digit \_ \']* { \s -> TokenSym s }
{
data Token 
  = TokenLet
  | TokenAdd
  | TokenStrict
  | TokenRec
  | TokenIf
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
  | TokenEOF
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
