{
module Main (main) where
}

%wrapper "posn"  -- adds a useful wrapper (alexScanTokens) around the basic
		 -- scanner, providing support for token positions and
		 -- continuations.

$digit = 0-9			-- digits
$alpha = [a-zA-Z]		-- alphabetic characters

tokens :-

  $white+				{ skip }
  "--".*				{ skip }
  let					{ tok (\p s -> Let p) }
  in					{ tok (\p s -> In p) }
  $digit+				{ tok (\p s -> Int p (read s)) }
  [\=\+\-\*\/\(\)]			{ tok (\p s -> Sym p (head s)) }
  $alpha [$alpha $digit \_ \']*		{ tok (\p s -> Var p s) }

{
-- Each right-hand side has type :: AlexPosn -> String -> [Token] -> [Token]

-- Some action helpers:
tok f p s rest = f p s : rest
skip  p s rest = rest

-- The token type:
data Token =
	Let AlexPosn		|
	In  AlexPosn		|
	Sym AlexPosn Char	|
	Var AlexPosn String	|
	Int AlexPosn Int	|
	Err AlexPosn
	deriving (Eq,Show)

main = do
  s <- getContents
  print (alexScanTokens s)
}
