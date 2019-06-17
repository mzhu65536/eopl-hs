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
    "begin" { TokenBegin  }
    "end"   { TokenEnd    }
    "set"   { TokenSet    }
    "if"    { TokenIf     }
    "else"  { TokenElse   }
    "then"  { TokenThen   }
    "zero?" { TokenZeroP  }
    let     { TokenLet    }
    rec     { TokenRec    }
    true    { TokenTrue   }
    false   { TokenFalse  }
    '$'     { TokenStrict }
    in      { TokenIn     }

    '\\'    { TokenLambda }
    '\"'    { TokenDQuote }
    "->"    { TokenArrow  }
    '='     { TokenEq     }
    '-'     { TokenSub    }
    '+'     { TokenAdd    }
    '*'     { TokenMul    }
    '('     { TokenLParen }
    ')'     { TokenRParen }
    '{'     { TokenLBrace }
    '}'     { TokenRBrace }    
    ','     { TokenCSep   }
    ';'     { TokenSSep   }
    "print" { TokenPrint  }
    "while" { TokenWhile  }
    "var"   { TokenVar    }   
    "cons"  { TokenCons   }
    "car"   { TokenCar    }
    "cdr"   { TokenCdr    }
    "null"  { TokenNil    }
    "null?" { TokenNilP   }
    "list"  { TokenList   }
    "try"   { TokenTry    }
    "catch" { TokenCatch  }
    "raise" { TokenRaise  }
    "div"   { TokenDiv    }
    NUM     { TokenNum $$ }
    VAR     { TokenSym $$ }
-- Operators
%left '+' '-'
%left '*' '$'
%%

-- Stms : Stmt { [$1] }
--      | Stmt ';' Stms { $1 : $3 }

-- Stmt : VAR '=' Expr                      { Assign $1 $3     }
--      | "print" Expr                      { Print $2         }
--      | '{' Stms '}'                      { Block $2         }
--      | "if" '(' Expr ')' Stmt Stmt       { IfS $3 $5 $6     }
--      | "while" '(' Expr ')' Stmt         { While $3 $5      }
--      | "var" Vars ';' Stmt               { Declare $2 $4    }
     

Expr : let VExs in Expr                  { Let $2 $4         }
     | rec VAR '(' Vars ')' '=' Expr in Expr      { Rec $2 $4 $7 $9   }
     | '\\' Vars "->" Expr               { Lam $2 $4         }
     | "if" Expr "then" Expr "else" Expr { If $2 $4 $6       }
     | "zero?" Expr                      { ZeroP $2          }
     | "set" VAR '=' Expr                { Set $2 $4         }
     | "begin" Exps "end"                { Begin $2          }
     | "try" Expr "catch" '(' VAR ')' Expr { Try $2 $5 $7    }
     | "raise" LFrm                       { Raise $2          }
     | LFrm                              { $1                }

VExp : VAR '=' Expr                      { ($1, $3)          }

VExs : VExp ',' VExs                     { $1 : $3           }
     | VExp                              { [$1]              }

Exps : Expr ',' Exps                     { $1 : $3           }
     | Expr                              { [$1]              }


Vars : VAR Vars                          { $1 : $2           }
     | VAR                               { [$1]              }
     
LFrm : "car" LFrm                        { Lst $ Car $2      }
     | "cdr" LFrm                        { Lst $ Cdr $2      }
     | "null?" LFrm                      { Lst $ NilP $2     }
     | "cons" Atom  LFrm                 { Lst $ Cons $2 $3  }
     | "list" '(' Exprs ')'              { Lst (Lsts $3)     }
     | Form                              { $1                }

Exprs :: { [Exp]                                             }
      : Expr                             { [$1]              }
      | Expr ',' Exprs                   { $1 : $3           }

Form : Form '+' Form                     { Op Plus $1 $3     }
     | Form '-' Form                     { Op Diff $1 $3     }
     | Form '*' Form                     { Op Mult $1 $3     }
     | Form "div" Form                   { Op Div $1 $3     }     
     | Fact                              { $1                }

Fact : Fact Atom                         { App $1 $2         }
     | Fact '$' Atom                     { App $1 $3         }
     | Atom                              { $1                }


Atom : '(' Expr ')'                      { $2                }
     | NUM                               { Lit (LInt $1)     }
     | '\"' VAR '\"'                     { Lit (LStr $2)     }
     | VAR                               { Var $1            }   
     | true                              { Lit (LBool True)  }
     | false                             { Lit (LBool False) }
     | "null"                            { Lst Nil           }

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
