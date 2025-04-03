{

-- Issue #269
-- reported 2025-04-02 by https://github.com/nicuveo
-- fixed 2025-04-03 by Antoine Leblanc
--
-- Problem was:
--   The minimizer was not initialized with the proper subsets of
--   states, and could result in different states being erroneously
--   considered equivalent, which in turn could resulting in the wrong
--   rule being selected at runtime.
--
-- This version of the test fails with the minimizer as implemented
-- after the changes in 4f0b51b8c370d0dd0f9c65af98282789f1cb035f.

import Control.Monad (when)
import System.Exit

}

%wrapper "basic"

tokens :-
  [abc]  { Left }
  "abc"  { const $ Right "abc" }

{

test :: String -> [Either String String] -> IO ()
test input expected =
  when (expected /= alexScanTokens input)
    exitFailure

main :: IO ()
main = do
  test "abc" [Right "abc"]
  test "bbb" [Left "b", Left "b", Left "b"]
  test "bbc" [Left "b", Left "b", Left "c"]

}
