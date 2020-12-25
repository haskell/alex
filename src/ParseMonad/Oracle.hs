{-# LANGUAGE GeneralisedNewtypeDeriving #-}
module ParseMonad.Oracle
  ( AlexInput (..)
  , StartCode (..)
  , setStartCode
  , getStartCode
  , PBase (..)
  ) where

import ParseMonad.Class

import Control.Applicative
import Control.Monad
import Control.Monad.State
import Control.Monad.Except

data StartCode
  = StartCode_0
  | StartCode_StartCodes
  | StartCode_AfterStartCodes
  deriving (Eq, Ord)

data AlexInput = AlexInput
  { curPos    :: AlexPosn     -- ^ current position
  , input     :: String       -- current input string
  , startCode :: StartCode
  }

alexPeekChar :: AlexInput -> Maybe (Char)
alexPeekChar (AlexInput { input = [] }) = Nothing
alexPeekChar (AlexInput { input = c : _ })  = Just c

alexGetChar :: AlexInput -> Maybe (Char,AlexInput)
alexGetChar (AlexInput { input = [] }) = Nothing
alexGetChar i@(AlexInput { curPos = p, input = c : s }) =
    p' `seq` Just (c, i { curPos = p', input = s })
  where p' = alexMove p c

newtype PBase a = PBase { unPBase :: StateT AlexInput (Either ParseError) a }
  deriving (Functor, Applicative, Monad)

instance Alternative PBase where
  empty = failPBase "invalid Char"
  PBase x <|> PBase y = PBase $ StateT $ \s ->
    case runStateT x s of
      Left _ -> runStateT y s
      Right res -> Right res

instance MonadPlus PBase

instance MonadBasicParse PBase where
  peekPBase = alexPeekChar <$> PBase get
  curPosPBase = do
    p <- curPos <$> PBase get
    return p
  nextCharPBase = do
    s <- PBase get
    case alexGetChar s of
      Nothing -> return Nothing
      Just (c, s') -> do
        PBase $ put s'
        return $ Just c

  raisePBase = PBase . throwError
  runPBase str p = fst <$> runStateT (unPBase p) initialState
    where initialState = AlexInput
            { curPos = alexStartPos
            , input = str
            , startCode = StartCode_0
            }

setStartCode :: StartCode -> PBase ()
setStartCode sc = PBase $ modify $ \s -> s { startCode = sc }

getStartCode :: PBase StartCode
getStartCode = PBase $ startCode <$> get
