{
-- Issue #197
-- reported 2022-01-21 by https://github.com/Commelina
-- fixed 2022-01-23 by Andreas Abel & John Ericson
--
-- Problem was:
-- Surface syntax regressed and could no longer handle character strings
-- that looked like numbers.

module Main (main) where

import System.Exit
}

%wrapper "posn"
%token   "Token"

@iec60559suffix = (32|64|128)[x]?
@any            = [01-89]+[x]?

:-

$white+         ;
@iec60559suffix { \ _ -> Good }
@any            { \ _ -> Bad }

{
data Token = Good String | Bad String
  deriving (Eq, Show)

input           = "32 32x 99 99x 128x"
expected_result = [Good "32", Good "32x", Bad "99", Bad "99x", Good "128x"]

main :: IO ()
main
  | result == expected_result = do
      exitWith ExitSuccess
  | otherwise = do
      print result
      exitFailure
  where
  result = alexScanTokens input
}
