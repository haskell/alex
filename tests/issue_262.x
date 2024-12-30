{
-- https://github.com/haskell/alex/pull/262
-- https://gitlab.haskell.org/ghc/ghc/-/issues/25609
--
-- Error happens when using alexScan with a lexer that
-- inspects the context.

{-# LANGUAGE ScopedTypeVariables #-}

import Control.Exception
import Data.List (isInfixOf)
}

%wrapper "basic"

:-
.* / { \state _ _ _ -> state == 'x' } { id }

{
main :: IO ()
main = do
  result <- try $ evaluate $ alexScan ('\n', [], "") 0 `seq` ()
  case result of
    Left (e :: SomeException)
      | "use alexScanUser instead" `isInfixOf` show e
      -> pure ()
    _ -> error $ "Got unexpected result: " ++ show result
}
