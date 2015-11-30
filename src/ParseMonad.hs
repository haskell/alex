-- -----------------------------------------------------------------------------
-- 
-- ParseMonad.hs, part of Alex
--
-- (c) Simon Marlow 2003
--
-- ----------------------------------------------------------------------------}

module ParseMonad (
        AlexInput, alexInputPrevChar, alexGetChar, alexGetByte,
        AlexPosn(..), alexStartPos,
 
        P, runP, StartCode, failP, lookupSMac, lookupRMac, newSMac, newRMac,
        setStartCode, getStartCode, getInput, setInput,
 ) where

import AbsSyn hiding ( StartCode )
import CharSet ( CharSet )
import Map ( Map )
import qualified Map hiding ( Map )
import UTF8

#if __GLASGOW_HASKELL__ < 710
import Control.Applicative ( Applicative(..) )
#endif
import Control.Monad ( liftM, ap )
import Data.Word (Word8)
-- -----------------------------------------------------------------------------
-- The input type
--import Codec.Binary.UTF8.Light as UTF8

type Byte = Word8

type AlexInput = (AlexPosn,     -- current position,
                  Char,         -- previous char
                  [Byte],
                  String)       -- current input string

alexInputPrevChar :: AlexInput -> Char
alexInputPrevChar (_,c,_,_) = c


alexGetChar :: AlexInput -> Maybe (Char,AlexInput)
alexGetChar (_,_,[],[]) = Nothing
alexGetChar (p,_,[],(c:s))  = let p' = alexMove p c in p' `seq`
                                Just (c, (p', c, [], s))
alexGetChar (_, _ ,_ : _, _) = undefined -- hide compiler warning

alexGetByte :: AlexInput -> Maybe (Byte,AlexInput)
alexGetByte (p,c,(b:bs),s) = Just (b,(p,c,bs,s))
alexGetByte (_,_,[],[]) = Nothing
alexGetByte (p,_,[],(c:s))  = let p' = alexMove p c 
                                  (b:bs) = UTF8.encode c
                              in p' `seq`  Just (b, (p', c, bs, s))

-- -----------------------------------------------------------------------------
-- Token positions

-- `Posn' records the location of a token in the input text.  It has three
-- fields: the address (number of chacaters preceding the token), line number
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
-- Alex lexing/parsing monad

type ParseError = (Maybe AlexPosn, String)
type StartCode = Int

data PState = PState {
                smac_env  :: Map String CharSet,
                rmac_env  :: Map String RExp,
                startcode :: Int,
                input     :: AlexInput
             }

newtype P a = P { unP :: PState -> Either ParseError (PState,a) }

instance Functor P where
  fmap = liftM

instance Applicative P where
  pure a = P $ \env -> Right (env,a)
  (<*>) = ap

instance Monad P where
 (P m) >>= k = P $ \env -> case m env of
                        Left err -> Left err
                        Right (env',ok) -> unP (k ok) env'
 return = pure

runP :: String -> (Map String CharSet, Map String RExp) 
        -> P a -> Either ParseError a
runP str (senv,renv) (P p) 
  = case p initial_state of
        Left err -> Left err
        Right (_,a) -> Right a
 where initial_state = 
          PState{ smac_env=senv, rmac_env=renv,
             startcode = 0, input=(alexStartPos,'\n',[],str) }

failP :: String -> P a
failP str = P $ \PState{ input = (p,_,_,_) } -> Left (Just p,str)

-- Macros are expanded during parsing, to simplify the abstract
-- syntax.  The parsing monad passes around two environments mapping
-- macro names to sets and regexps respectively.

lookupSMac :: (AlexPosn,String) -> P CharSet
lookupSMac (posn,smac)
 = P $ \s@PState{ smac_env = senv } -> 
       case Map.lookup smac senv of
        Just ok -> Right (s,ok)
        Nothing -> Left (Just posn, "unknown set macro: $" ++ smac)

lookupRMac :: String -> P RExp
lookupRMac rmac 
 = P $ \s@PState{ rmac_env = renv } -> 
       case Map.lookup rmac renv of
        Just ok -> Right (s,ok)
        Nothing -> Left (Nothing, "unknown regex macro: %" ++ rmac)

newSMac :: String -> CharSet -> P ()
newSMac smac set 
  = P $ \s -> Right (s{smac_env = Map.insert smac set (smac_env s)}, ())

newRMac :: String -> RExp -> P ()
newRMac rmac rexp 
  = P $ \s -> Right (s{rmac_env = Map.insert rmac rexp (rmac_env s)}, ())

setStartCode :: StartCode -> P ()
setStartCode sc = P $ \s -> Right (s{ startcode = sc }, ())

getStartCode :: P StartCode
getStartCode = P $ \s -> Right (s, startcode s)

getInput :: P AlexInput
getInput = P $ \s -> Right (s, input s)

setInput :: AlexInput -> P ()
setInput inp = P $ \s -> Right (s{ input = inp }, ())
