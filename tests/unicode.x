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

<0> {
   "αω"        { string }
   [AΓ]        { character }
   .           { other }
}


{
string :: AlexInput -> Int -> Alex String
string (_,_,_,_) _ = return "string!"

other :: AlexInput -> Int -> Alex String
other (_,_,_,input) len = return (take len input)

character :: AlexInput -> Int -> Alex String
character (_,_,_,_) _ = return "PING!"

alexEOF :: Alex String
alexEOF = return "stopped."

scanner :: String -> Either String [String]
scanner str = runAlex str $ do
  let loop = do tok <- alexMonadScan
		if tok == "stopped." || tok == "error."
			then return [tok]
			else do toks <- loop
				return (tok:toks)
  loop

main :: IO ()
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

  let test3 = scanner str3
  when (test3 /= out3) $
	do hPutStrLn stderr "Test 3 failed:"
	   print test3
	   exitFailure

  let test4 = scanner str4
  when (test4 /= out4) $
	do hPutStrLn stderr "Test 4 failed:"
	   print test4
	   exitFailure



str1 = "A."
out1 = Right ["PING!",".","stopped."]

str2 = "\n"
out2 = Left "lexical error at line 1, column 1"


str3 = "αω --"
out3 = Right ["string!"," ","-","-","stopped."]

str4 = "βΓ"
out4 = Right ["β","PING!","stopped."]

}
