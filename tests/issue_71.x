{
-- Issue #71
-- reported 2015-10-20 by Ian Duncan
-- fixed 2020-01-22 by Andreas Abel
--
-- Problem was:
-- DFA minimization crashed with "Prelude head: empty list" because
-- empty set of non-accepting states was treated as empty equivalence
-- class of states.
--
-- Issue #258, 2024-02-27, Andreas Abel:
-- Since GHC 9.4, type 'Symbol' conflicts with 'GHC.Exts.Symbol'
-- which was imported by alex <= 3.5.0.0
-- because of an unqualified import of 'GHC.Exts'.

module Main (main) where

import System.Exit
}

%wrapper "posn"
%token   "Symbol"

$whitespace  = [\ \n\t]
@whitespaces = $whitespace*

:-

@whitespaces { \ _ _ -> Whitespaces }
"a"          { \ _ _ -> A           }

{
-- Calling the token type 'Symbol' will trigger a clash with GHC.Exts.Symbol
-- if the lexer is built with alex <= 3.5.0.0.

data Symbol = Whitespaces | A
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
