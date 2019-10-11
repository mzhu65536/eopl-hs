{
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Parser.Typed.Parser (
  parseExprTyped,
  parseTokensTyped,
) where

import Parser.Typed.Lexer
import Parser.Typed.Syntax

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
    "begin" { TokenBegin    }
    "end"   { TokenEnd      }
    "set"   { TokenSet      }
    "if"    { TokenIf       }
    "else"  { TokenElse     }
    "then"  { TokenThen     }
    "zero?" { TokenZeroP    }
    let     { TokenLet      }
    rec     { TokenRec      }
    true    { TokenTrue     }
    false   { TokenFalse    }
    '$'     { TokenStrict   }
    in      { TokenIn       }

    deref   { TokenDeref    }
    newref  { TokenNewref   }
    setref  { TokenSetref   }

    "pair!" { TokenPairM    }
    left    { TokenLeftM    }
    right   { TokenRightM   }
    setleft { TokenSetLeft  }
    setright{ TokenSetRight }
    
    '\\'    { TokenLambda   }
    '\"'    { TokenDQuote   }
    "->"    { TokenArrow    }
    '='     { TokenEq       }
    '-'     { TokenSub      }
    '+'     { TokenAdd      }
    '*'     { TokenMul      }
    '('     { TokenLParen   }
    ')'     { TokenRParen   }
    '{'     { TokenLBrace   }
    '}'     { TokenRBrace   }
    '['     { TokenLBrakt   }
    ']'     { TokenRBrakt   }    
    ','     { TokenCSep     }
    ';'     { TokenSSep     }
    ':'     { TokenColon    }    
    "print" { TokenPrint    }
    "while" { TokenWhile    }
    "var"   { TokenVar      }   
    "cons"  { TokenCons     }
    "car"   { TokenCar      }
    "cdr"   { TokenCdr      }
    "null"  { TokenNil      }
    "null?" { TokenNilP     }
    "list"  { TokenList     }
    "try"   { TokenTry      }
    "catch" { TokenCatch    }
    "raise" { TokenRaise    }
    "int"   { TokenInt      }
    "bool"  { TokenBool     }
    "str"   { TokenString   }    
    "div"   { TokenDiv      }
    "pair"  { TokenPair     }
    "unpair"{ TokenUnPair   }
    "pairof"{ TokenPairOf   }
    "listof"{ TokenListOf   }    
    NUM     { TokenNum $$   }
    VAR     { TokenSym $$   }
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
     


-- refined Syntax for new Typed language
Expr : let Asgs in Expr                     { Let $2 $4              }
     | rec Funs in Expr                     { Rec $2 $4              }
     | '\\' Args "->" Expr                  { Lam $2 $4              }
     | "if" Expr "then" Expr "else" Expr    { If $2 $4 $6            }
     | "zero?" Expr                         { ZeroP $2               }
     | "set" VAR '=' Expr                   { Set $2 $4              }
     | "begin" Exps "end"                   { Begin $2               }
     | "try" Expr "catch" '(' VAR ')' Expr  { Try $2 $5 $7           }
     | "raise" Expr                         { Raise $2               }
     | LFrm                                 { $1                     }
     | MutP                                 { $1                     }
     | ERef                                 { $1                     }
-- EXPLICIT-REFS Language Extension     
ERef : newref Expr                          { ERef $ ERNew $2        }
     | setref VAR  '=' Expr                 { ERef $ ERSet $2 $4     }
     | deref Expr                           { ERef $ ERDer $2        }
MutP : "pair!" Atom Atom                    { MPar $ MPNew $2 $3     }
     | left Atom                            { MPar $ MPLeft $2       }
     | right Atom                           { MPar $ MPRight $2      }
     | setleft Atom Atom                    { MPar $ MPSetL $2 $3    }
     | setright Atom Atom                   { MPar $ MPSetR $2 $3    }
Func : Type VAR Args '=' Expr               { (($2, $1), $3, $5)     }
Funs : Func                                 { [$1]                   }
     | Func ',' Funs                        { $1 : $3                }
Asgs : Assg                                 { [$1]                   }
     | Assg ',' Asgs                        { $1 : $3                }
Assg : VAR '=' Expr                         { ($1, $3)               }
Argu : '(' VAR ':' Type ')'                 { ($2, $4)               }
Args : Argu                                 { [$1]                   }
     | Argu Args                            { $1 : $2                }
Type : "int"                                { LTInt                  }
     | "str"                                { LTString               } 
     | "bool"                               { LTBool                 }
     | "null"                               { LTEmpty                }
     | "pairof" Type Type                   { LTPair $2 $3           }
     | "listof" Type                        { LTList $2              }     
     | Type "->" Type                       { LTCurry $1 $3          }
     | '(' Type ')'                         { $2                     }

LFrm : "car" LFrm                           { Lst $ Car $2           }
     | "cdr" LFrm                           { Lst $ Cdr $2           }
     | "null?" LFrm                         { Lst $ NilP $2          }
     | "cons" Atom  LFrm                    { Lst $ Cons $2 $3       }
     | '[' Exps ']'                         { Lst (Lsts $2)          }
     | "pair" '(' Expr ',' Expr ')'         { LPr $ LPCons $3 $5       }
     | "unpair" VAR VAR '=' Expr in Expr    { LPr $ LPDStr $2 $3 $5 $7 }    
     | Form                                 { $1                     }

Exps :: { [Exp] }
     : Expr                              { [$1]              }
     | Expr ',' Exps                     { $1 : $3           }

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
     | "null" ':' Type                   { Lst $ Nil $3      }

{

parseError :: [Token] -> Except String a
parseError (l:ls) = throwError (show l)
parseError [] = throwError "Unexpected end of Input"

parseExprTyped :: String -> Either String Exp
parseExprTyped input = runExcept $ do
  tokenStream <- scanTokens input
  expr tokenStream

parseTokensTyped :: String -> Either String [Token]
parseTokensTyped = runExcept . scanTokens
    
}
