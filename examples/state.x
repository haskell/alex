%{
import Alex
%}

"state_lx"/"state_acts":-

  <>     ::= ^w+
  <code> ::= ^%^{ (~^% | ^%~^})* ^%^}
  <ide>  ::= [A-Za-z]+


%{
code _ _ inp len cont (sc,frags) = cont (sc,frag:frags)
	where
	frag = take (len-4) (drop 2 inp)

ide _ _ inp len cont st = Ide (take len inp):cont st


data Token = Ide String | Eof String | Err 	deriving Show


main:: IO ()
main = interact (show.tokens)

tokens:: String -> [Token]
tokens inp = gscan state_scan [] inp

state_scan:: GScan [String] [Token]
state_scan = load_gscan (state_acts,stop_act) state_lx
	where
	stop_act _ _ "" (_,frags) = [Eof (unlines(reverse frags))]
	stop_act _ _ _ _ = [Err]
%}
