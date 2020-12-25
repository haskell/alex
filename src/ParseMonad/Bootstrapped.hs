{-# LANGUAGE GeneralisedNewtypeDeriving #-}
module ParseMonad.Bootstrapped
  ( alexGetByte
  , PBase
  , alexInputPrevChar
  , alexGetChar
  , AlexInput (..)
  , Byte
  , getInput
  , setInput
  , StartCode
  , setStartCode
  , getStartCode
  ) where

import UTF8
import ParseMonad.Class

#if __GLASGOW_HASKELL__ < 710
import Control.Applicative ( Applicative(..) )
#endif
import Control.Monad.State
import Control.Monad.Except
import Data.Word (Word8)

-- -----------------------------------------------------------------------------
-- The input type
--import Codec.Binary.UTF8.Light as UTF8

type Byte = Word8
type StartCode = Int

data AlexInput = AlexInput
  { curPos    :: AlexPosn     -- ^ current position
  , prevChar  :: Char         -- ^ previous char
  , bytes     :: [Byte]
  , input     :: String       -- current input string
  , startCode :: StartCode
  }

alexInputPrevChar :: AlexInput -> Char
alexInputPrevChar = prevChar

alexPeekChar :: AlexInput -> Maybe (Char)
alexPeekChar AlexInput { bytes = [], input = [] } = Nothing
alexPeekChar AlexInput { bytes = [], input = c : _ }  = Just c
alexPeekChar AlexInput { bytes = _ : _ } = undefined -- hide compiler warning

alexGetChar :: AlexInput -> Maybe (Char, AlexInput)
alexGetChar AlexInput { bytes = [], input = [] } = Nothing
alexGetChar i@(AlexInput { bytes = [], input = c : cs, curPos = p }) =
    p' `seq` Just (c, i { curPos = p', prevChar = c, input = cs })
  where p' = alexMove p c
alexGetChar AlexInput { bytes = _ : _ } = undefined -- hide compiler warning

alexGetByte :: AlexInput -> Maybe (Byte, AlexInput)
alexGetByte AlexInput { bytes = [], input = [] } = Nothing
alexGetByte i@(AlexInput { bytes = b : bs }) = Just (b, i { bytes = bs })
alexGetByte i@(AlexInput { bytes = [], input = c : cs, curPos = p }) =
    p' `seq` Just (b, i { curPos = p', prevChar = c, bytes = bs, input = cs })
  where
    p' = alexMove p c
    (b : bs) = UTF8.encode c

-- -----------------------------------------------------------------------------
-- The underlying monad, for lexing

newtype PBase a = PBase { unPBase :: StateT AlexInput (Either ParseError) a }
  deriving (Functor, Applicative, Monad)

instance MonadBasicParse PBase where
  peekPBase = alexPeekChar <$> getInput
  curPosPBase = curPos <$> getInput
  nextCharPBase = do
    s <- getInput
    case alexGetChar s of
      Nothing -> return Nothing
      Just (c, s') -> do
        setInput s'
        return $ Just c

  raisePBase = PBase . throwError
  runPBase str p = fst <$> runStateT (unPBase p) initialState
    where initialState = AlexInput
            { curPos    = alexStartPos
            , prevChar  = '\n'
            , bytes     = []
            , input     = str
            , startCode = 0
            }

getInput :: PBase AlexInput
getInput = PBase get

setInput :: AlexInput -> PBase ()
setInput = PBase . put

setStartCode :: StartCode -> PBase ()
setStartCode sc = PBase $ modify $ \s -> s { startCode = sc }

getStartCode :: PBase StartCode
getStartCode = PBase $ startCode <$> get
