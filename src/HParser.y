{
module HParser (parse) where

import Syntax
import Alexer


}

%name toplevel
%tokentype { PosToken }

%token
    LAMBDA { PosToken (Lambda) _ }
    '.' { PosToken Dot _ }
    '(' { PosToken (Sym '(') _ }
    ')' { PosToken (Sym ')') _ }
    ';' { PosToken (Sym ';') _ }
    IDENTIFIER { PosToken _ _}

%%

TopLevel :: { [Command] }
TopLevel :
    Command { [$1] } |
    TopLevel ';' Command { $3:$1 } |
    {- empty -} { [] }

Command :: { Command }
Command :
    Term { Eval (termInfo $1) $1 }

Term :: { Term }
Term :
    AppTerm { $1 } |
    LAMBDA IDENTIFIER '.' Term { Abs (info $1) (getId $2) $4 }

AppTerm :: { Term }
AppTerm :
    ATerm { $1 } |
    AppTerm ATerm { App (termInfo $1) $1 $2 }

ATerm :: { Term }
ATerm :
    '(' Term ')' { $2 } |
    IDENTIFIER { Var (info $1) (getId $1) }

{
info (PosToken _ pos) = Info pos
token (PosToken tok _) = tok
  
getId (PosToken (Id s) _) = s

happyError x = error ("Parse Error at line " ++ show x)

parse :: [PosToken] -> [Command]
parse = toplevel
}
