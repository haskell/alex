-------------------------------------------------------------------------------
--                  ALEX SCANNER AND LITERATE PREPROCESSOR
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
module Scan (lexer) where

--import Debug.Trace

import ParseMonad
import Token
}

$digit    = 0-9
$hexdig   = [0-9 A-F a-f]
$octal    = 0-7
$lower    = a-z
$upper    = A-Z
$alpha    = [$upper $lower]
$alphanum = [$alpha $digit]
$idchar   = [$alphanum \_ \']

$special    = [\.\;\,\$\|\*\+\?\#\~\-\{\}\(\)\[\]\^\/]
$graphic    = $printable # $white
$nonspecial = $graphic # [$special \%]

@id     = $alpha $idchar*
@smac   = \$ @id | \$ \{ @id \}
@rmac   = \@ @id | \@ \{ @id \}

@comment = "--".*
@ws      = $white+ | @comment

alex :-

@ws                             { skip }     -- white space; ignore

<0> \" [^\"]* \"                { string }
<0> (@id @ws?)? \:\-            { bind }
<0> \{ / (\n | [^$digit])       { code }
<0> $special                    { special }  -- note: matches {
<0> \% "wrapper"                { wrapper }
<0> \% "encoding"               { encoding }
<0> \% "action"                 { actionty }
<0> \% "token"                  { tokenty }
<0> \% "typeclass"              { typeclass }

<0> \\ $digit+                  { decch }
<0> \\ x $hexdig+               { hexch }
<0> \\ o $octal+                { octch }
<0> \\ $printable               { escape }
<0> $nonspecial # [\<]          { char } -- includes 1 digit numbers
<0> $digit+                     { num  } -- should be after char
<0> @smac                       { smac }
<0> @rmac                       { rmac }

<0> @smac @ws? \=               { smacdef }
<0> @rmac @ws? \=               { rmacdef }

-- identifiers are allowed to be unquoted in startcode lists
<0>             \<              { special `andBegin` startcodes }
<startcodes>    0               { zero }
<startcodes>    @id             { startcode }
<startcodes>    \,              { special }
<startcodes>    \>              { special `andBegin` afterstartcodes }

-- After a <..> startcode sequence, we can have a {...} grouping of rules,
-- so don't try to interpret the opening { as a code block.
<afterstartcodes> \{ (\n | [^$digit ])  { special `andBegin` 0 }
<afterstartcodes> ()            { skip `andBegin` 0 }  -- note: empty pattern
{

-- -----------------------------------------------------------------------------
-- Token functions

lexer :: (Token -> P a) -> P a
lexer cont = lexToken >>= cont

lexToken :: P Token
lexToken = do
  inp@(p,c,_,s) <- getInput
  sc <- getStartCode
  case alexScan inp sc of
    AlexEOF -> return (T p EOFT)
    AlexError _ -> lexError "lexical error"
    AlexSkip inp1 _ -> do
      setInput inp1
      lexToken
    AlexToken inp1 len t -> do
      setInput inp1
      t (p,c,s) len

skip :: Action
skip _ _ = lexToken
}
