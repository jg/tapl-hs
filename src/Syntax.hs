module Syntax where

data Command =
  Eval Info Term |
  Bind Info String String deriving (Eq, Show)

data Term =
  Var Info String     |
  Abs Info String Term |
  App Info Term Term deriving (Eq, Show)

data Info = Info Position deriving (Eq, Show)

termInfo :: Term -> Info
termInfo (Var info _) = info
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
