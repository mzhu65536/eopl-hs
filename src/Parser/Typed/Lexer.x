{
{-# LANGUAGE FlexibleContexts #-}

module Parser.Typed.Lexer (
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

  "pair!"                       { \_ -> TokenPairM    }
  left                          { \_ -> TokenLeftM    }
  right                         { \_ -> TokenRightM   }
  setleft                       { \_ -> TokenSetLeft  }
  setright                      { \_ -> TokenSetRight }

  deref                         { \_ -> TokenDeref  }
  newref                        { \_ -> TokenNewref }
  setref                        { \_ -> TokenSetref }


  "int"                         { \_ -> TokenInt}
  "bool"                        { \_ -> TokenBool}
  "str"                         { \_ -> TokenString}  

  "->"                          { \_ -> TokenArrow  }
  \=                            { \_ -> TokenEq     }
  \\                            { \_ -> TokenLambda }
  [\-]                          { \_ -> TokenSub    }
  [\*]                          { \_ -> TokenMul    }
  "$"                           { \_ -> TokenStrict }
  [\+]                          { \_ -> TokenAdd    }  
  \(                            { \_ -> TokenLParen }
  \)                            { \_ -> TokenRParen }
  "{"                           { \_ -> TokenLBrace }
  "}"                           { \_ -> TokenRBrace }
  "["                           { \_ -> TokenLBrakt }
  "]"                           { \_ -> TokenRBrakt }  
  ","                           { \_ -> TokenCSep   }
  ";"                           { \_ -> TokenSCep   }
  ":"                           { \_ -> TokenColon  }  
  ["]                           { \_ -> TokenDQuote }     
  -- List Extension
  "cons"                        { \_ -> TokenCons }
  "car"                         { \_ -> TokenCar  }
  "cdr"                         { \_ -> TokenCdr  }
  "null"                        { \_ -> TokenNil  }
  "null?"                       { \_ -> TokenNilP }
  "list"                        { \_ -> TokenList }
  "pair"                        { \_ -> TokenPair   }
  "unpair"                      { \_ -> TokenUnPair }
  "pairof"                      { \_ -> TokenPairOf }
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
  | TokenDeref  
  | TokenNewref 
  | TokenSetref 
  | TokenAdd
  | TokenSet
  | TokenDeclare
  | TokenCatch
  | TokenTry
  | TokenRaise
  | TokenPrint
  | TokenWhile
  | TokenSCep
  | TokenPairM   
  | TokenLeftM   
  | TokenRightM  
  | TokenSetLeft 
  | TokenSetRight
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
  | TokenLBrakt
  | TokenRBrakt
  | TokenEOF
  | TokenDiv
  | TokenDQuote
  | TokenCons
  | TokenCar 
  | TokenCdr 
  | TokenNil 
  | TokenNilP
  | TokenList
  | TokenBool
  | TokenString
  | TokenInt
  | TokenColon
  | TokenPair
  | TokenUnPair
  | TokenPairOf
  | TokenListOf
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
