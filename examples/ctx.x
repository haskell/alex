%{
import Alex
%}


{ ^A = A-Z       }
{ %t = [^ ^t]*^n }

"ctx_lx"/"ctx_acts":-

  <only_ide>  ::=   ^n\^A+/%t		%{ only_ide  = tkn 0    %}
  <start_ide> ::=   ^n\^A+		%{ start_ide = tkn 1    %}
  <end_ide>   ::=      ^A+/%t		%{ end_ide   = tkn 2    %}
  <ide>       ::=      ^A+		%{ ide       = tkn 3    %}
  <tricky>    ::=      x*/x		%{ tricky    = tkn 4    %}
  <dot>       ::=  "0":^.		%{ dot       = tkn 5    %}
  <open>      ::=      ^(		%{ open      = start op %}
  <comma>     ::= "op":^,		%{ comma     = tkn 6    %}
  <close>     ::=      ^)		%{ close     = start 0  %}
  <>          ::=      [.^n]


%{
tkn n _ _ inp len cont st = Ide n (take len inp):cont st

start sc _ _ _ _ cont (_,s) = cont (sc,s)


main:: IO ()
main = interact (show.tokens)

data Tkn = Ide Int String     deriving Show

tokens:: String -> [Tkn]
tokens inp = gscan ctx_scan () inp

ctx_scan:: GScan () [Tkn]
ctx_scan = load_gscan (ctx_acts,stop_act) ctx_lx
	where
	stop_act _ _ "" _ = []
	stop_act _ _ _  _ = error "tokens"
%}
