%{
module Tokens where

import Alex
%}


{ ^d = 0-9      }			-- digits
{ ^l = [a-zA-Z] }			-- alphabetic characters

"tokens_lx"/"tokens_acts":-

  <>     ::=  ^w+			-- white space
  <>     ::=  ^-^-.*			-- comments
  <let'> ::=  let			%{ let' p s = Let p          %}
  <in'>  ::=  in			%{ in'  p s = In  p          %}
  <int>  ::=  ^d+			%{ int  p s = Int p (read s) %}	
  <sym>  ::=  `=+-*/()'			%{ sym  p s = Sym p (head s) %}
  <var>  ::=  ^l[^l^d^_^']*		%{ var  p s = Var p s        %}


%{
data Token =
	Let Posn		|
	In  Posn		|
	Sym Posn Char		|
	Var Posn String		|
	Int Posn Int		|
	Err Posn
	deriving (Eq,Show)

token_pos:: Token -> Posn
token_pos (Let p)   = p
token_pos (In  p)   = p
token_pos (Sym p _) = p
token_pos (Var p _) = p
token_pos (Int p _) = p
token_pos (Err p)   = p


tokens:: String -> [Token]
tokens inp = scan tokens_scan inp

tokens_scan:: Scan Token
tokens_scan = load_scan (tokens_acts,stop_act) tokens_lx
	where
	stop_act p ""  = []
	stop_act p inp = [Err p]
%}
