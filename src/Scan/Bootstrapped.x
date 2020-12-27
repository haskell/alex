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
module Scan.Bootstrapped (lexToken) where

--import Debug.Trace

import ParseMonad
import ParseMonad.Bootstrapped
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

<0> \" [^\"]* \"                { liftAct string }
<0> (@id @ws?)? \:\-            { liftAct bind }
<0> \{ / (\n | [^$digit])       { \(p, _, _) _ -> T p <$> code }
<0> $special                    { liftAct special }  -- note: matches {
<0> \% "wrapper"                { liftAct wrapper }
<0> \% "encoding"               { liftAct encoding }
<0> \% "action"                 { liftAct actionty }
<0> \% "token"                  { liftAct tokenty }
<0> \% "typeclass"              { liftAct typeclass }

<0> \\ $digit+                  { liftAct decch }
<0> \\ x $hexdig+               { liftAct hexch }
<0> \\ o $octal+                { liftAct octch }
<0> \\ $printable               { liftAct escape }
<0> $nonspecial # [\<]          { liftAct char } -- includes 1 digit numbers
<0> $digit+                     { liftAct num  } -- should be after char
<0> @smac                       { liftAct smac }
<0> @rmac                       { liftAct rmac }

<0> @smac @ws? \=               { liftAct smacdef }
<0> @rmac @ws? \=               { liftAct rmacdef }

-- identifiers are allowed to be unquoted in startcode lists
<0>             \<              { liftAct special `andBegin` startcodes }
<startcodes>    0               { liftAct zero }
<startcodes>    @id             { liftAct startcode }
<startcodes>    \,              { liftAct special }
<startcodes>    \>              { liftAct special `andBegin` afterstartcodes }

-- After a <..> startcode sequence, we can have a {...} grouping of rules,
-- so don't try to interpret the opening { as a code block.
<afterstartcodes> \{ (\n | [^$digit ])  { liftAct special `andBegin` 0 }
<afterstartcodes> ()            { skip `andBegin` 0 }  -- note: empty pattern
{

-- -----------------------------------------------------------------------------
-- Token functions

special, zero, string, bind, escape, decch, hexch, octch, char,
  num, smac, rmac, smacdef, rmacdef, startcode, wrapper, encoding,
  actionty, tokenty, typeclass
  :: Action
special   str _  = SpecialT $ head str
zero      _   _  = ZeroT
string    str ln = StringT $ extract ln str
bind      str _  = BindT $ takeWhile isIdChar str
escape    str _  = CharT $ esc $ head $ tail str
decch     str ln = CharT $ do_ech 10 $ take (ln-1) (tail str)
hexch     str ln = CharT $ do_ech 16 $ take (ln-2) (drop 2 str)
octch     str ln = CharT $ do_ech 8  $ take (ln-2) (drop 2 str)
char      str _  = CharT $ head str
num       str ln = NumT $ parseInt 10 $ take ln str
smac      str ln = SMacT $ mac ln str
rmac      str ln = RMacT $ mac ln str
smacdef   str _  = SMacDefT $ macdef str
rmacdef   str _  = RMacDefT $ macdef str
startcode str ln = IdT $ take ln str
wrapper   _   _  = WrapperT
encoding  _   _  = EncodingT
actionty  _   _  = ActionTypeT
tokenty   _   _  = TokenTypeT
typeclass _   _  = TypeClassT

-- -----------------------------------------------------------------------------
-- Lexer

lexToken :: PBase Token
lexToken = do
  inp@(AlexInput p c _ s _) <- getInput
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

skip :: Action'
skip _ _ = lexToken

liftAct :: Action -> Action'
liftAct a (p, _, s) len = return $ T p $ a s len

andBegin :: Action' -> StartCode -> Action'
andBegin act sc inp len = setStartCode sc >> act inp len

type Action = String -> Int -> Tkn
type Action' = (AlexPosn, Char, String) -> Int -> PBase Token

}
