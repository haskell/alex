{
-- Tests the basic operation.
module Main where

import Data.Char (toUpper)
import Control.Monad
import System.Exit
import System.IO
}

%wrapper "monad"

@word = [A-Za-z]+

tokens :-

$white+			{ skip }

<0> "magic"		{ magic } -- should override later patterns
<0> @word $		{ eol }  -- test trailing context
<0> ^ @word		{ bol }  -- test left context
<0> @word		{ word }

<0> \(			{ begin parens }
<parens> [A-Za-z]+	{ parenword }
<parens> \)		{ begin 0 }

{
{- we can now have comments in source code? -}
word (p,_,input) len = return (take len input)

eol (p,_,input) len = return ("EOL:"++ take len input)

bol (p,_,input) len = return ("BOL:"++ take len input)

parenword (p,_,input) len = return (map toUpper (take len input))

magic (p,_,input) len = return "PING!"

alexEOF (p,_,"")   = return "stopped."
alexEOF (p,_,rest) = alexError "error."

scanner str = runAlex str $ do
  let loop = do tok <- alexMonadScan
		if tok == "stopped." || tok == "error." 
			then return [tok]
			else do toks <- loop
				return (tok:toks)
  loop  

main = do
  let test1 = scanner str1
  when (test1 /= out1) $ 
	do hPutStrLn stderr "Test 1 failed:"
	   print test1
	   exitFailure

  let test2 = scanner str2
  when (test2 /= out2) $
	do hPutStrLn stderr "Test 2 failed:"
	   print test2
	   exitFailure

str1 = "a b c (d e f) magic (magic) eol\nbol"
out1 = Right ["BOL:a","b","c","D","E","F","PING!","MAGIC","EOL:eol", "BOL:bol", "stopped."]

str2 = "."
out2 = Left "error."
}
