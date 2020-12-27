{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
-- -----------------------------------------------------------------------------
--
-- ParseMonad.hs, part of Alex
--
-- (c) Simon Marlow 2003
--
-- ----------------------------------------------------------------------------}

module ParseMonad.Class (
        AlexPosn(..), alexStartPos, alexMove,
        ParseError, MonadBasicParse (..), failPBase,
 ) where

#if __GLASGOW_HASKELL__ < 710
import Control.Applicative ( Applicative(..) )
#endif

-- -----------------------------------------------------------------------------
-- Token positions

-- `Posn' records the location of a token in the input text.  It has three
-- fields: the address (number of charaters preceding the token), line number
-- and column of a token within the file. `start_pos' gives the position of the
-- start of the file and `eof_pos' a standard encoding for the end of file.
-- `move_pos' calculates the new position after traversing a given character,
-- assuming the usual eight character tab stops.

data AlexPosn = AlexPn !Int !Int !Int
        deriving (Eq,Show)

alexStartPos :: AlexPosn
alexStartPos = AlexPn 0 1 1

alexMove :: AlexPosn -> Char -> AlexPosn
alexMove (AlexPn a l c) '\t' = AlexPn (a+1)  l     (((c+7) `div` 8)*8+1)
alexMove (AlexPn a l _) '\n' = AlexPn (a+1) (l+1)   1
alexMove (AlexPn a l c) _    = AlexPn (a+1)  l     (c+1)

-- -----------------------------------------------------------------------------
-- Alex lexing / basic parsing monad class

type ParseError = (Maybe AlexPosn, String)

class Monad m => MonadBasicParse m where
  curPosPBase :: m AlexPosn
  peekPBase :: m (Maybe Char)
  nextCharPBase :: m (Maybe Char)

  raisePBase :: ParseError -> m a
  runPBase :: String -> m a -> Either ParseError a

failPBase :: MonadBasicParse m => String -> m a
failPBase str = do
  l <- curPosPBase
  raisePBase (Just l, str)
