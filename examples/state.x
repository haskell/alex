{
module Main (main) where
}

%wrapper "gscan"

state :-

  $white+	{ skip }
  \{ [^\}]* \}  { code }
  [A-Za-z]+	{ ide }

{
code _ _ inp len cont (sc,frags) = cont (sc,frag:frags)
	where
	frag = take (len-4) (drop 2 inp)

ide _ _ inp len cont st = Ide (take len inp):cont st

skip _ _ inp len cont st = cont st

data Token = Ide String | Eof String | Err 	deriving Show

stop_act _ _ "" (_,frags) = [Eof (unlines(reverse frags))]
stop_act _ _ _ _ = [Err]

tokens:: String -> [Token]
tokens inp = alexGScan stop_act [] inp

main:: IO ()
main = interact (show.tokens)
}
