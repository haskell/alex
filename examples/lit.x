{
module Main (main) where
}

%wrapper "gscan"

$space   = $white # \n
@blank   = \n $space*
@scrap   = \n \> .*
@comment = \n ( [^ \> $white] | $space+ ~$white ) .*

lit :-
 
   @blank @scrap+		{ scrap }
   @blank @comment*		{ comment }
 
{
scrap _ _ inp len cont st = strip len inp
	where
	strip 0 _ = cont st
	strip (n+1) (c:rst) =
		if c=='\n'
		   then '\n':strip_nl n rst
		   else c:strip n rst

	strip_nl (n+1) ('>':rst) = ' ':strip n rst
	strip_nl n rst = strip n rst

comment _ _ inp len cont st = strip len inp
	where
	strip 0 _ = cont st
	strip (n+1) (c:rst) = if c=='\n' then c:strip n rst else strip n rst


main:: IO ()
main = interact literate

literate:: String -> String
literate inp = drop 2 (alexGScan stop_act () ('\n':'\n':inp))

stop_act p _ "" st = []
stop_act p _ _  _  = error (msg ++ loc p ++ "\n")

msg  = "literate preprocessing error at "

loc (AlexPn _ l c) = "line " ++ show(l-2) ++ ", column " ++ show c
}
