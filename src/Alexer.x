{
module Alexer (scan, test, Token(..), AlexPosn(..), PosToken(..)) where
}

%wrapper "posn"

$digit = 0-9			-- digits
$alpha = [a-zA-Z]		-- alphabetic characters

tokens :-

  $white+				;
  "--".*				;
  "."                                   { tok (\s -> Dot) }
  "lambda"                              { tok (\s -> Lambda) }
  $digit+				{ tok (\s -> Int (read s)) }
  [\=\+\-\*\/\(\)]			{ tok (\s -> Sym (head s)) }
  $alpha [$alpha $digit \_ \']*		{ tok (\s -> Var s) }
{

-- Each right-hand side has type :: AlexPosn -> String -> Token
-- Some action helpers:

tok :: (String -> Token) -> AlexPosn -> String -> PosToken
tok f (AlexPn abs line col) s =
  PosToken (f s) (Position line col)

-- The token type:
data Token =
    Sym Char	|
    Var String	|
    Int Int     |
    Lambda      |
    Dot 
    deriving (Eq,Show)


data Position = Position Int Int deriving (Eq, Show)

data PosToken = PosToken Token Position deriving (Eq, Show)

scan s = alexScanTokens s


-- return tokens without their positions. Good for testing
test s = fmap pickToken (alexScanTokens s)
  where pickToken (PosToken token position) = token

}
 
