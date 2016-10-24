module Syntax where

data Command =
  Eval Term |
  Bind String String deriving (Eq, Show)

data Term =
  Var String     |
  Abs String Term |
  App Term Term deriving (Eq, Show)


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
