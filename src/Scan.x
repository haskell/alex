-------------------------------------------------------------------------------
--		    ALEX SCANNER AND LITERATE PREPROCESSOR
-- 
-- This Script defines the grammar used to generate the Alex scanner and a
-- preprocessing scanner for dealing with literate scripts.  The actions for
-- the Alex scanner are given separately in the Alex module.
--  
-- See the Alex manual for a discussion of the scanners defined here.
--  
-- Chris Dornan, Aug-95, 4-Jun-96, 10-Jul-96, 29-Sep-97
-------------------------------------------------------------------------------

{
module Scan(lexer, AlexPosn(..), Token(..), Tkn(..), tokPosn) where

import Data.Char
import ParseMonad
--import Debug.Trace
}

$digit    = 0-9
$hexdig   = [0-9 A-F a-f]
$octal    = 0-7
$lower    = a-z
$upper    = A-Z
$alpha    = [$upper $lower]
$alphanum = [$alpha $digit]
$idchar   = [$alphanum \_ \']

$special    = [\.\;\,\$\|\*\+\?\#\~\-\{\}\(\)\[\]\^]
$graphic    = $printable # $white
$nonspecial = $graphic # [$special \%]

@id     = $alpha $idchar*
@smac   = \$ @id | \$ \{ @id \}
@rmac   = \@ @id | \@ \{ @id \}

@comment = "--".*
@ws      = $white+ | @comment

alex :-

@ws				{ skip }	-- white space; ignore

<0> \" [^\"]* \"		{ string }
<0> @id @ws? \:\-		{ bind }
<0> \{ [^$digit]		{ code }
<0> $special			{ special }  -- note: matches {
<0> \% "wrapper"		{ wrapper }

<0> \\ $digit+			{ decch }
<0> \\ x $hexdig+		{ hexch }
<0> \\ o $octal+		{ octch }
<0> \\ $printable		{ escape }
<0> $nonspecial # [\<]		{ char }
<0> @smac			{ smac }
<0> @rmac			{ rmac }

<0> @smac @ws? \=		{ smacdef }
<0> @rmac @ws? \=		{ rmacdef }

-- identifiers are allowed to be unquoted in startcode lists
<0> 		\< 		{ special `andBegin` startcodes }
<startcodes>	0		{ zero }
<startcodes>	@id		{ startcode }
<startcodes>	\,		{ special }
<startcodes> 	\> 		{ special `andBegin` afterstartcodes }

-- After a <..> startcode sequence, we can have a {...} grouping of rules,
-- so don't try to interpret the opening { as a code block.
<afterstartcodes> \{ [^$digit ]  { special `andBegin` 0 }
<afterstartcodes> ()		{ skip `andBegin` 0 }  -- note: empty pattern
{

-- -----------------------------------------------------------------------------
-- Token type

data Token = T AlexPosn Tkn
  deriving Show

tokPosn (T p _) = p

data Tkn
 = SpecialT Char
 | CodeT String
 | ZeroT
 | IdT String
 | StringT String
 | BindT String
 | CharT Char
 | SMacT String
 | RMacT String  
 | SMacDefT String
 | RMacDefT String  
 | NumT Int	
 | WrapperT
 | EOFT
 deriving Show

-- -----------------------------------------------------------------------------
-- Token functions

special   (p,_,str) ln = return $ T p (SpecialT  (head str))
zero      (p,_,str) ln = return $ T p ZeroT
string    (p,_,str) ln = return $ T p (StringT (extract ln str))
bind      (p,_,str) ln = return $ T p (BindT (takeWhile isIdChar str))
escape    (p,_,str) ln = return $ T p (CharT (esc str))
decch     (p,_,str) ln = return $ T p (CharT (do_ech 10 ln (take (ln-1) (tail str))))
hexch     (p,_,str) ln = return $ T p (CharT (do_ech 16 ln (take (ln-2) (drop 2 str))))
octch     (p,_,str) ln = return $ T p (CharT (do_ech 8  ln (take (ln-2) (drop 2 str))))
char      (p,_,str) ln = return $ T p (CharT (head str))
smac      (p,_,str) ln = return $ T p (SMacT (mac ln str))
rmac      (p,_,str) ln = return $ T p (RMacT (mac ln str))
smacdef   (p,_,str) ln = return $ T p (SMacDefT (macdef ln str))
rmacdef   (p,_,str) ln = return $ T p (RMacDefT (macdef ln str))
startcode (p,_,str) ln = return $ T p (IdT (take ln str))
wrapper   (p,_,str) ln = return $ T p WrapperT

isIdChar c = isAlphaNum c || c `elem` "_'"

extract ln str = take (ln-2) (tail str)
		
do_ech radix ln str = chr (parseInt radix str)

mac ln (_ : str) = take (ln-1) str

macdef ln (_ : str) = takeWhile (not.isSpace) str

esc (_ : x : _)  =
 case x of
   'a' -> '\a'
   'b' -> '\b'
   'f' -> '\f'
   'n' -> '\n'
   'r' -> '\r'
   't' -> '\t'
   'v' -> '\v'
   c   ->  c

parseInt :: Int -> String -> Int
parseInt radix ds = foldl1 (\n d -> n * radix + d) (map digitToInt ds)

-- In brace-delimited code, we have to be careful to match braces
-- within the code, but ignore braces inside strings and character
-- literals.  We do an approximate job (doing it properly requires
-- implementing a large chunk of the Haskell lexical syntax).

code (p,_,inp) len = do
 inp <- getInput
 go inp 1 ""
 where
  go inp 0 cs = do
    setInput inp
    return (T p (CodeT (reverse (tail cs))))
  go inp n cs = do
    case alexGetChar inp of
	Nothing  -> err inp
	Just (c,inp)   -> 
	  case c of
		'{'  -> go inp (n+1) (c:cs) 
		'}'  -> go inp (n-1) (c:cs)
		'\'' -> go_char inp n (c:cs)
		'\"' -> go_str inp n (c:cs) '\"'
		c    -> go inp n (c:cs)

	-- try to catch occurrences of ' within an identifier
  go_char inp n (c1:c2:cs) | isAlphaNum c2 = go inp n (c1:c2:cs)
  go_char inp n cs = go_str inp n cs '\''

  go_str inp n cs end = do
    case alexGetChar inp of
	Nothing -> err inp
	Just (c,inp)
	  | c == end  -> go inp n (c:cs)
	  | otherwise -> 
		case c of
		   '\\' -> case alexGetChar inp of
			     Nothing -> err inp
			     Just (d,inp)  -> go_str inp n (d:c:cs) end
		   c -> go_str inp n (c:cs) end

  err inp = do setInput inp; lexError "lexical error in code fragment"
				  


lexError s = do
  (p,_,input) <- getInput
  failP (s ++ (if (not (null input))
		  then " at " ++ show (head input)
		  else " at end of file"))

lexer :: (Token -> P a) -> P a
lexer cont = lexToken >>= cont

lexToken :: P Token
lexToken = do
  inp <- getInput
  sc <- getStartCode
  case alexScan inp sc of
    Left _ -> case inp of
		 (p,_,"")   -> return (T p EOFT)
		 (p,_,rest) -> lexError "lexical error"
    Right (inp1,len,t) -> do
	setInput inp1
	t inp len

type Action = AlexInput -> Int -> P Token

skip :: Action
skip _ _ = lexToken

andBegin :: Action -> StartCode -> Action
andBegin act sc inp len = setStartCode sc >> act inp len
}
