-- Performance test; run with input /usr/dict/words, for example
{
module Main (main) where
}

%wrapper "posn"

words :-

$white+			;
[A-Za-z0-9\'\-]+	{ \p s -> () }

{
main = do
 s <- getContents
 print (length (alexScanTokens s))
}
