--
-- Lexical syntax for Haskell 98.
--
-- (c) Simon Marlow 2003, with the caveat that much of this is
-- translated directly from the syntax in the Haskell 98 report.
--

{
module Main (main) where
import Data.Char ( ord )
}

%wrapper "monad"

$whitechar = [ \t\n\r\f\v]
$special   = [\(\)\,\;\[\]\`\{\}]

$ascdigit  = 0-9
$unidigit  = [] -- TODO
$digit     = [$ascdigit $unidigit]

$ascsymbol = [\!\#\$\%\&\*\+\.\/\<\=\>\?\@\\\^\|\-\~]
$unisymbol = [] -- TODO
$symbol    = [$ascsymbol $unisymbol] # [$special \_\:\"\']

$large     = [A-Z \xc0-\xd6 \xd8-\xde]
$small     = [a-z \xdf-\xf6 \xf8-\xff \_]
$alpha     = [$small $large]

$graphic   = [$small $large $symbol $digit $special \:\"\']

$octit	   = 0-7
$hexit     = [0-9 A-F a-f]
$idchar    = [$alpha $digit \']
$symchar   = [$symbol \:]
$nl        = [\n\r]

@reservedid = 
	as|case|class|data|default|deriving|do|else|hiding|if|
	import|in|infix|infixl|infixr|instance|let|module|newtype|
	of|qualified|then|type|where

@reservedop =
	".." | ":" | "::" | "=" | \\ | "|" | "<-" | "->" | "@" | "~" | "=>"

@varid  = $small $idchar*
@conid  = $large $idchar*
@varsym = $symbol $symchar*
@consym = \: $symchar*

@decimal     = $digit+
@octal       = $octit+
@hexadecimal = $hexit+
@exponent    = [eE] [\-\+] @decimal

$cntrl   = [$large \@\[\\\]\^\_]
@ascii   = \^ $cntrl | NUL | SOH | STX | ETX | EOT | ENQ | ACK
	 | BEL | BS | HT | LF | VT | FF | CR | SO | SI | DLE
	 | DC1 | DC2 | DC3 | DC4 | NAK | SYN | ETB | CAN | EM
	 | SUB | ESC | FS | GS | RS | US | SP | DEL
$charesc = [abfnrtv\\\"\'\&]
@escape  = \\ ($charesc | @ascii | @decimal | o @octal | x @hexadecimal)
@gap     = \\ $whitechar+ \\
@string  = $graphic # [\"\\] | " " | @escape | @gap

haskell :-

<0> $white+			{ skip }
<0> "--"\-*[^$symbol].*		{ skip }

"{-"				{ nested_comment }

<0> $special			{ mkL LSpecial }

<0> @reservedid			{ mkL LReservedId }
<0> @conid \. @varid		{ mkL LQVarId }
<0> @conid \. @conid		{ mkL LQConId }
<0> @varid			{ mkL LVarId }
<0> @conid			{ mkL LConId }

<0> @reservedop			{ mkL LReservedOp }
<0> @conid \. @varsym		{ mkL LVarSym }
<0> @conid \. @consym		{ mkL LConSym }
<0> @varsym			{ mkL LVarSym }
<0> @consym			{ mkL LConSym }

<0> @decimal 
  | 0[oO] @octal
  | 0[xX] @hexadecimal		{ mkL LInteger }

<0> @decimal \. @decimal @exponent?
  | @decimal @exponent		{ mkL LFloat }

<0> \' ($graphic # [\'\\] | " " | @escape) \'
				{ mkL LChar }

<0> \" @string* \"		{ mkL LString }

{
data Lexeme = L AlexPosn LexemeClass String

data LexemeClass
  = LInteger
  | LFloat
  | LChar
  | LString
  | LSpecial
  | LReservedId
  | LReservedOp
  | LVarId
  | LQVarId
  | LConId
  | LQConId
  | LVarSym
  | LQVarSym
  | LConSym
  | LQConSym
  | LEOF
  deriving Eq
  
mkL :: LexemeClass -> AlexInput -> Int -> Alex Lexeme
mkL c (p,_,str) len = return (L p c (take len str))

nested_comment :: AlexInput -> Int -> Alex Lexeme
nested_comment _ _ = do
  input <- alexGetInput
  go 1 input
  where go 0 input = do alexSetInput input; alexMonadScan
	go n input = do
	  case alexGetChar input of
	    Nothing  -> err input
	    Just (c,input) -> do
	      case c of
	    	'-' -> do
		  case alexGetChar input of
		    Nothing  -> err input
		    Just ('\125',input) -> go (n-1) input
		    Just (c,input)      -> go n input
	     	'\123' -> do
		  case alexGetChar input of
		    Nothing  -> err input
		    Just ('-',input) -> go (n+1) input
		    Just (c,input)   -> go n input
	    	c -> go n input

        err input = do alexSetInput input; lexError "error in nested comment"  

lexError s = do
  (p,c,input) <- alexGetInput
  alexError (showPosn p ++ ": " ++ s ++ 
		   (if (not (null input))
		     then " before " ++ show (head input)
		     else " at end of file"))

scanner str = runAlex str $ do
  let loop i = do tok@(L _ cl _) <- alexMonadScan; 
		  if cl == LEOF
			then return i
			else do loop $! (i+1)
  loop 0

alexEOF (p,_,"")   = return (L p LEOF "")
alexEOF (p,_,rest) = lexError "lexical error"

showPosn (AlexPn _ line col) = show line ++ ':': show col

main = do
  s <- getContents
  print (scanner s)
}
