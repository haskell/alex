{
-- Issue #71
-- reported 2015-10-20 by Ian Duncan
-- fixed 2020-01-22 by Andreas Abel
--
-- Problem was:
-- DFA minimization crashed with "Prelude head: empty list" because
-- empty set of non-accepting states was treated as empty equivalence
-- class of states.

module Main (main) where

import System.Exit
}

%wrapper "posn"
%token   "Token"

$whitespace  = [\ \n\t]
@whitespaces = $whitespace*

:-

@whitespaces { \ _ _ -> Whitespaces }
"a"          { \ _ _ -> A           }

{
data Token = Whitespaces | A
  deriving (Eq, Show)

input           = "aa    \n\taa   \t  \n  a"
expected_result = [A,A,Whitespaces,A,A,Whitespaces,A]

main :: IO ()
main
    -- Since the whitespaces token is nullable, Alex
    -- will recognize an infinite number of those
    -- at the end of file.  This behavior is problematic,
    -- but we don't fix it here.
    -- We just test here whether the expected result
    -- is a prefix of the produced result.
  | take (length expected_result) result == expected_result = do
      exitWith ExitSuccess
  | otherwise = do
      print $ take 20 result
      exitFailure
  where
  result = alexScanTokens input
}
