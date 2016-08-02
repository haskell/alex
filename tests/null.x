{
-- Tests the basic operation.
module Main where

import Data.Char (toUpper)
import Control.Monad
import System.Exit
import System.IO
import Prelude hiding (null)
}

%wrapper "monad"

@word = [A-Za-z]+
@null = \0

$escchars = [abfnrtv\\"\'&]
@escape = \\ ($escchars | \0)
@gap = \\ $white+ \\
@string = $printable # [\"] | " " | @escape | @gap

@inComment = ([^\*] | $white)+ | ([\*]+ ([\x00-\xff] # [\/]))

tokens :-

$white+			;

<0> {
   @null                { null }
   @word                { word }
   \" @string \"        { string }
   "--" @inComment \n   { word }
}

{
{- we can now have comments in source code? -}
word (_,_,_,input) len = return (take len input)

null (_,_,_,_) _ = return "\0"

string (_,_,_,input) _ = return (drop 1 (reverse (drop 1 (reverse input))))

alexEOF = return "stopped."

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

str1 = "a\0bb\0ccc\0\0\"\\\0\""
out1 = Right ["a","\NUL","bb","\NUL","ccc","\NUL","\NUL","\\\NUL", "stopped."]

str2 = "."
out2 = Left "lexical error at line 1, column 1"
}
