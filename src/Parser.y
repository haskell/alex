{
-- -----------------------------------------------------------------------------
--
-- Parser.y, part of Alex
--
-- (c) Simon Marlow 2003
--
-- -----------------------------------------------------------------------------

{-# OPTIONS_GHC -w #-}

module Parser ( parse, P ) where
import AbsSyn
import Scan
import CharSet
import ParseMonad hiding ( StartCode )

import Data.Char
--import Debug.Trace
}

%tokentype { Token }

%name parse

%monad { P } { (>>=) } { return }
%lexer { lexer } { T _ EOFT }

%token
	'.'		{ T _ (SpecialT '.') }
	';'		{ T _ (SpecialT ';') }
	'<'		{ T _ (SpecialT '<') }
	'>'		{ T _ (SpecialT '>') }
	','		{ T _ (SpecialT ',') }
	'$'		{ T _ (SpecialT '$') }
	'|'		{ T _ (SpecialT '|') }
	'*'		{ T _ (SpecialT '*') }
	'+'		{ T _ (SpecialT '+') }
	'?'		{ T _ (SpecialT '?') }
	'{'		{ T _ (SpecialT '{') }
	'}'		{ T _ (SpecialT '}') }
	'('		{ T _ (SpecialT '(') }
	')'		{ T _ (SpecialT ')') }
	'#'		{ T _ (SpecialT '#') }
	'~'		{ T _ (SpecialT '~') }
	'-'		{ T _ (SpecialT '-') }
	'['		{ T _ (SpecialT '[') }
	']'		{ T _ (SpecialT ']') }
	'^'		{ T _ (SpecialT '^') }
	'/'		{ T _ (SpecialT '/') }
	ZERO		{ T _ ZeroT }
	STRING		{ T _ (StringT $$) }
	BIND		{ T _ (BindT $$) }
	ID		{ T _ (IdT $$) }
	CODE		{ T _ (CodeT _) }
	CHAR		{ T _ (CharT $$) }
	NUM		{ T _ (NumT $$) }
	SMAC		{ T _ (SMacT _) }
	RMAC		{ T _ (RMacT $$) }
	SMAC_DEF	{ T _ (SMacDefT $$) }
	RMAC_DEF	{ T _ (RMacDefT $$) }
	WRAPPER		{ T _ WrapperT }
	ENCODING	{ T _ EncodingT }
        ACTIONTYPE      { T _ ActionTypeT }
        TOKENTYPE       { T _ TokenTypeT }
        TYPECLASS       { T _ TypeClassT }
%%

alex	:: { (Maybe (AlexPosn,Code), [Directive], Scanner, Maybe (AlexPosn,Code)) }
	: maybe_code directives macdefs scanner maybe_code { ($1,$2,$4,$5) }

maybe_code :: { Maybe (AlexPosn,Code) }
	: CODE				{ case $1 of T pos (CodeT code) ->
						Just (pos,code) }
	| {- empty -}			{ Nothing }

directives :: { [Directive] }
	: directive directives		{ $1 : $2 }
	| {- empty -}			{ [] }

directive  :: { Directive }
	: WRAPPER STRING		{ WrapperDirective $2 }
	| ENCODING encoding		{ EncodingDirective $2 }
        | ACTIONTYPE STRING             { ActionType $2 }
        | TOKENTYPE STRING              { TokenType $2 }
        | TYPECLASS STRING              { TypeClass $2 }

encoding :: { Encoding }
        : STRING         		{% lookupEncoding $1 }

macdefs :: { () }
	: macdef macdefs		{ () }
	| {- empty -}			{ () }

-- hack: the lexer looks for the '=' in a macro definition, because there
-- doesn't seem to be a way to formulate the grammar here to avoid a
-- conflict (it needs LR(2) rather than LR(1) to find the '=' and distinguish
-- an SMAC/RMAC at the beginning of a definition from an SMAC/RMAC that is
-- part of a regexp in the previous definition).
macdef	:: { () }
	: SMAC_DEF set			{% newSMac $1 $2 }
	| RMAC_DEF rexp			{% newRMac $1 $2 }

scanner	:: { Scanner }
	: BIND tokendefs	 	{ Scanner $1 $2 }

tokendefs :: { [RECtx] }
	: tokendef tokendefs		{ $1 ++ $2 }
	| {- empty -}			{ [] }

tokendef :: { [RECtx] }
	: startcodes rule		{ [ replaceCodes $1 (snd $2) ] }
	| startcodes '{' rules '}'	{ map (replaceCodes $1) $3 }
	| rule				{% do
                                           let (pos, res@(RECtx _ _ e _ _)) = $1
                                           warnIfNullable e pos
                                           return [ res ]
                                        }

rule    :: { (AlexPosn, RECtx) }
        : context rhs                   { let
                                            (l, e, r)   = $1
                                            (pos, code) = $2
                                          in (pos, RECtx [] l e r code)
                                        }

rules	:: { [RECtx] }
	: rule rules			{ snd $1 : $2 }
	| {- empty -}			{ [] }

startcodes :: { [(String,StartCode)] }
	: '<' startcodes0 '>' 		{ $2 }

startcodes0 :: { [(String,StartCode)] }
	: startcode ',' startcodes0 	{ ($1,0) : $3 }
	| startcode 			{ [($1,0)] }

startcode :: { String }
	: ZERO 				{ "0" }
	| ID	 			{ $1 }

rhs	:: { (AlexPosn, Maybe Code) }
	: CODE 				{ case $1 of T pos (CodeT code) -> (pos, Just code) }
	| ';'				{ (tokPosn $1, Nothing) }

context :: { Maybe CharSet, RExp, RightContext RExp }
	: left_ctx rexp right_ctx	{ (Just $1,$2,$3) }
	| rexp right_ctx		{ (Nothing,$1,$2) }

left_ctx :: { CharSet }
	: '^'				{ charSetSingleton '\n' }
	| set '^' 			{ $1 }

right_ctx :: { RightContext RExp }
	: '$'		{ RightContextRExp (Ch (charSetSingleton '\n')) }
	| '/' rexp	{ RightContextRExp $2 }
        | '/' CODE	{ RightContextCode (case $2 of
						T _ (CodeT code) -> code) }
	| {- empty -}	{ NoRightContext }

rexp	:: { RExp }
	: alt '|' rexp 			{ $1 :|| $3 }
	| alt		 		{ $1 }

alt	:: { RExp }
	: alt term  			{ $1 :%% $2 }
	| term 				{ $1 }

term	:: { RExp }
	: rexp0 rep 			{ $2 $1 }
	| rexp0 			{ $1 }

rep	:: { RExp -> RExp }
	: '*' 				{ Star }
	| '+' 				{ Plus }
	| '?' 				{ Ques }
	| begin_mult '{' mult '}'	{ $3 }
-- A bit counterintuitively, we need @begin_mult@ already before the left brace,
-- not just before @mult@.  This might be due to the lookahead in the parser.

-- Enter the "multiplicity" lexer mode to scan number literals
begin_mult :: { () }
	: {- empty -}			{% setStartCode multiplicity }

-- Parse a numeric multiplicity.
mult	:: { RExp -> RExp }
	: NUM				{ repeat_rng $1 Nothing }
	| NUM ','			{ repeat_rng $1 (Just Nothing) }
	| NUM ',' NUM			{ repeat_rng $1 (Just (Just $3)) }

rexp0	:: { RExp }
	: '(' ')'  			{ Eps }
	| STRING			{ foldr (:%%) Eps
					    (map (Ch . charSetSingleton) $1) }
	| RMAC 				{% lookupRMac $1 }
	| set 				{ Ch $1 }
	| '(' rexp ')' 			{ $2 }

set	:: { CharSet }
 	: set '#' set0 			{ $1 `charSetMinus` $3 }
	| set0 				{ $1 }

set0	:: { CharSet }
	: CHAR 				{ charSetSingleton $1 }
	| CHAR '-' CHAR			{ charSetRange $1 $3 }
	| smac 				{% lookupSMac $1 }
	| '[' sets ']' 			{ foldr charSetUnion emptyCharSet $2 }

	-- [^sets] is the same as  '. # [sets]'
	-- The upshot is that [^set] does *not* match a newline character,
	-- which seems much more useful than just taking the complement.
	| '[' '^' sets ']'
			{% do { dot <- lookupSMac (tokPosn $1, ".");
		      	        return (dot `charSetMinus`
			      		  foldr charSetUnion emptyCharSet $3) }}

	-- ~set is the same as '. # set'
	| '~' set0	{% do { dot <- lookupSMac (tokPosn $1, ".");
		      	        return (dot `charSetMinus` $2) } }

sets	:: { [CharSet] }
	: set sets			{ $1 : $2 }
	| {- empty -}			{ [] }

smac	:: { (AlexPosn,String) }
 	: '.'				{ (tokPosn $1, ".") }
	| SMAC				{ case $1 of T p (SMacT s) -> (p, s) }

{
happyError :: P a
happyError = failP "parse error"

-- -----------------------------------------------------------------------------
-- Utils

digit c = ord c - ord '0'

repeat_rng :: Int -> Maybe (Maybe Int) -> (RExp->RExp)
repeat_rng n (Nothing) re = foldr (:%%) Eps (replicate n re)
repeat_rng n (Just Nothing) re = foldr (:%%) (Star re) (replicate n re)
repeat_rng n (Just (Just m)) re = intl :%% rst
	where
	intl = repeat_rng n Nothing re
	rst = foldr (\re re'->Ques(re :%% re')) Eps (replicate (m-n) re)

replaceCodes codes rectx = rectx{ reCtxStartCodes = codes }

lookupEncoding :: String -> P Encoding
lookupEncoding s = case map toLower s of
  "iso-8859-1" -> return Latin1
  "latin1"     -> return Latin1
  "utf-8"      -> return UTF8
  "utf8"       -> return UTF8
  _            -> failP ("encoding " ++ show s ++ " not supported")

}
