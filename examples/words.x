-- this is intended to be a performance test of an alex-generated lexer
-- * run with /usr/dict/words or equivalent, depending on your os, e.g.:
--   * mac os: /usr/share/dict/words
--   * alpine linux: available as a package
--     (https://pkgs.alpinelinux.org/packages?name=words&branch=edge&repo=&arch=&origin=&flagged=&maintainer=)
--     and when installed, available here: /usr/share/dict/ (the directory contains multiple languages)
-- * to generate lexer:
--   alex words.x
-- * to scan a basic string with the generated lexer using ghci:
--   $ ghci
--   ghci> :l words
--   ghci> alexScanTokens "word1 word2" (produces: ["word1","word2"])
-- * to run the performance test as intended, compile the lexer into an executable and run with:
--   time ./words.bin +RTS -s < /usr/dict/words
{
module Main (main) where
}

%wrapper "basic"

words :-

$white+			;
[A-Za-z0-9\'\-]+	{ \s -> s }

{
main = do
 s <- getContents
 print (length (alexScanTokens s))
}
