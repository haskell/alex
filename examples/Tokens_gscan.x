{
module Main (main) where
}

%wrapper "gscan"

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
-- Some action helpers:
tok f p c str len cont (sc,state) = f p (take len str) : cont (sc,state)

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
  print (alexGScan stop undefined s)
  where
	stop p c "" (sc,s) = []
	stop p c _  (sc,s) = error "lexical error"
}
