{
module Tokens_posn (Token(..), AlexPosn(..), alexScanTokens, token_posn) where
}

%wrapper "posn"

$digit = 0-9			-- digits
$alpha = [a-zA-Z]		-- alphabetic characters

tokens :-

  $white+				;
  "--".*				;
  let					{ tok (\p s -> Let p) }
  in					{ tok (\p s -> In p) }
  $digit+				{ tok (\p s -> Int p (read s)) }
  [\=\+\-\*\/\(\)]			{ tok (\p s -> Sym p (head s)) }
  $alpha [$alpha $digit \_ \']*		{ tok (\p s -> Var p s) }

{
-- Each right-hand side has type :: AlexPosn -> String -> Token

-- Some action helpers:
tok f p s = f p s

-- The token type:
data Token =
	Let AlexPosn		|
	In  AlexPosn		|
	Sym AlexPosn Char	|
	Var AlexPosn String	|
	Int AlexPosn Int
	deriving (Eq,Show)

token_posn (Let p) = p
token_posn (In p) = p
token_posn (Sym p _) = p
token_posn (Var p _) = p
token_posn (Int p _) = p
}
