-- Performance test; run with input /usr/dict/words, for example
{
module Main (main) where
}

%wrapper "monad"

words :-

$white+			{ skip }
[A-Za-z0-9\'\-]+	{ word }

{
word (_,_,input) len = return (take len input)

scanner str = runAlex str $ do
  let loop i = do tok <- alexMonadScan; 
		  if tok == "stopped." || tok == "error." 
			then return i
			else do let i' = i+1 in i' `seq` loop i'
  loop 0

alexEOF = return "stopped."

main = do
 s <- getContents
 print (scanner s)
}
