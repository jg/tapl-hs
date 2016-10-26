module Syntax where

data Command =
  Eval Info Term |
  Bind Info String String deriving (Eq, Show)

data Term =
  Var Info String Int Int | -- name, de brujin, context length
  Abs Info String Term |
  App Info Term Term deriving (Eq)

class Brujin a where
  brujin :: a -> String

instance Brujin Term where
  brujin (Var _ _ x _) = show x
  brujin (Abs _ _ t) = "L. " ++ brujin t
  brujin (App _ t1 t2) = brujin t1 ++ " " ++ brujin t2

instance Show Term where
  show (Var _ name x _) = "Var " ++ name ++ " " ++ show x
  show (Abs _ name t) = "(L " ++ name ++ ". " ++ show t ++ ")"
  show (App _ t1 t2) = "(" ++ show t1 ++ " " ++ show t2 ++ ")"

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

type TermMapFn =  Info -- need to pass info to construct a new Var
               -> String -- name of variable
               -> Int  -- threshold above which we shift (c)
               -> Int  -- de Brujin index (x)
               -> Int  -- context length (n)
               -> Term

-- | Notation is arrow d c where d is the amount we want to shift, c
-- is the integer threshold above which we actually want to shift
tmmap :: TermMapFn
      -> Int
      -> Term
      -> Term
tmmap onvar c term =
  let
    walk :: Int -- de Brujin index
         -> Term
         -> Term
    walk c' (Var info name x ctxlen) = onvar info name c' x ctxlen
    walk c' (Abs info name t2) = Abs info name (walk (c' + 1) t2)
    walk c' (App info t1 t2) = App info (walk c' t1) (walk c' t2)
  in walk c term

termShiftAbove :: Int -> Int -> Term -> Term
termShiftAbove d c t =
  tmmap shift c t
  where
    shift i name c' x n =
        if x >= c'
        then Var i name (x+d) (n+d)
        else Var i name x     (n+d)

termShift :: Int -> Term -> Term
termShift d t = termShiftAbove d 0 t

-- [j |-> s] t where j is de Brujin, s is a term
termSubst :: Int -> Term -> Term -> Term
termSubst j s t =
  tmmap subst 0 t
  where
    subst i name c x n =
        if x == j + c
        then termShift c s
        else Var i name x n

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
 
