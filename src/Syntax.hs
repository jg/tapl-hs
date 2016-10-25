module Syntax where

data Command =
  Eval Info Term |
  Bind Info String String deriving (Eq, Show)

data Term =
  Var Info String Int Int | -- name, de brujin, context length
  Abs Info String Term |
  App Info Term Term deriving (Eq, Show)

data Info = Info Position deriving (Eq, Show)

type Name = String
type Value = String
type Context = [(Name, Value)]

emptyContext :: Context
emptyContext = []


addName :: Context -> Name -> Context
addName ctx name = (name, "") : ctx

isNameBound :: Context -> Name -> Bool
isNameBound (x:xs) name =
  if fst x == name then True else isNameBound xs name

name2Index :: Info -> Context -> Name -> Int
name2Index info ctx x =
  case ctx of
    [] -> error $ "Identifier " ++ x ++ " is unbound"
    (y, _): ys -> if x == y then 0 else 1 + (name2Index info ys x)

termInfo :: Term -> Info
termInfo (Var info _ _ _) = info
termInfo (Abs info _ _) = info
termInfo (App info _ _) = info

-- The token type:
data Token =
    Sym Char        	|
    Id String	        |
    Int Int             |
    Lambda              |
    Dot 
    deriving (Eq, Show)


data Position = Position Int Int deriving (Eq, Show)

data PosToken = PosToken Token Position deriving (Eq, Show)
