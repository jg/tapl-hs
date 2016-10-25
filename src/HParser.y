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

TopLevel :: { Context -> [Command] }
TopLevel :
    Command { \ctx -> [$1 ctx] } |
    TopLevel ';' Command { \ctx -> $3 ctx : $1 ctx} |
    {- empty -} {  \ctx -> [] }

Command :: { Context -> Command }
Command :
    Term { \ctx -> Eval (termInfo ($1 ctx)) ($1 ctx) }

Term :: { Context -> Term }
Term :
    AppTerm { \ctx -> $1 ctx } |
    LAMBDA IDENTIFIER '.' Term {
        \ctx -> let
            inf = info $1
            name = getId $2
            ctx1 = addName ctx name
        in
            Abs (info $1) (getId $2) ($4 ctx1)
    }

AppTerm :: { Context -> Term }
AppTerm :
    ATerm { \ctx -> $1 ctx } |
    AppTerm ATerm { \ctx -> App (termInfo ($1 ctx)) ($1 ctx) ($2 ctx) }

ATerm :: { Context -> Term }
ATerm :
    '(' Term ')' { \ctx -> $2 ctx } |
    IDENTIFIER {
        \ctx -> let
            name = getId $1
            inf = info $1
            brujin = name2Index inf ctx name
            l = length ctx
        in      
            Var inf name brujin l
    }

{
info (PosToken _ pos) = Info pos
token (PosToken tok _) = tok
  
getId (PosToken (Id s) _) = s

happyError x = error ("Parse Error at line " ++ show x)

parse :: [PosToken] -> [Command]
parse = (flip toplevel) emptyContext
}
