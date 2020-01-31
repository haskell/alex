{
-- Issue #141
-- reported 2015-10-20 by Iavor S. Diatchki
-- fixed 2020-01-31 by Andreas Abel
--
-- Problem was:
-- Only one-digit numbers were accepted in repetition ranges.

module Main (main) where

import System.Exit
}

%wrapper "posn"
%token   "Token"

:-

-- allow several digits in repetition ranges, e.g. 14
"a"{14,14}          { \ _ _ -> A }
[\ \n\t]+           ;

{
data Token = A
  deriving (Eq, Show)

                -- 12345678901234
input           = "aaaaaaaaaaaaaa\n"  -- fourteen a's
expected_result = [A]

main :: IO ()
main
  | result == expected_result = do
      exitWith ExitSuccess
  | otherwise = do
      print $ take 20 result
      exitFailure
  where
  result = alexScanTokens input
}
