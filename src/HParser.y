{
module HParser (parse) where

import Syntax
import Alexer


}

%name toplevel
%tokentype { Token }

%token
    LAMBDA { Lambda }
    '.' { Dot }
    '(' { Sym '(' }
    ')' { Sym ')' }
    ';' { Sym ';' }
    IDENTIFIER { Id $$ }

%%

TopLevel :
    Command { [$1] } |
    TopLevel ';' Command { $3:$1 } |
    {- empty -} { [] }

Command : Term { Eval $1 }

Term :
    AppTerm { $1 } |
    LAMBDA IDENTIFIER '.' Term { Abs $2 $4 }

AppTerm :
    ATerm { $1 } |
    AppTerm ATerm { App $1 $2 }

ATerm :
    '(' Term ')' { $2 } |
    IDENTIFIER { Var $1 }
{

getId (Id s) = s

happyError x = error ("Parse Error at line " ++ show x)

parse :: [Token] -> [Command]
parse = toplevel
}
