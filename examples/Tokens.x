{
module Tokens (Token(..), alexScanTokens) where
}

%wrapper "basic"

$digit = 0-9			-- digits
$alpha = [a-zA-Z]		-- alphabetic characters

tokens :-

  $white+				{ \s -> White }
  "--".*				{ \s -> Comment }
  let					{ \s -> Let }
  in					{ \s -> In }
  $digit+				{ \s -> Int (read s) }
  [\=\+\-\*\/\(\)]			{ \s -> Sym (head s) }
  $alpha [$alpha $digit \_ \']*		{ \s -> Var s }

{
-- Each right-hand side has type :: String -> Token

-- The token type:
data Token =
        White		|
        Comment		|
	Let 		|
	In  		|
	Sym Char	|
	Var String	|
	Int Int		|
	Err 
	deriving (Eq,Show)
}
