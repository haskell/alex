{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
-- -----------------------------------------------------------------------------
--
-- ParseMonad.hs, part of Alex
--
-- (c) Simon Marlow 2003
--
-- ----------------------------------------------------------------------------}

module ParseMonad (
        module ParseMonad.Class,
        Warning(..), warnIfNullable,
        P, P', PBase, runP, raiseP, failP,
        lookupSMac, lookupRMac, newSMac, newRMac,
 ) where

import AbsSyn hiding ( StartCode )
import CharSet ( CharSet )
import Map ( Map )
import qualified Map hiding ( Map )
import ParseMonad.Class

import Control.Applicative
import Control.Monad.State ( StateT(..), get, modify )
import Control.Monad.Trans ( MonadTrans, lift )
import Control.Monad ( MonadPlus, when )

#if ALEX_BOOTSTRAP
import ParseMonad.Bootstrapped (PBase)
#else
import ParseMonad.Oracle (PBase)
#endif

-- -----------------------------------------------------------------------------
-- Alex parsing monad transformerx

data Warning
  = WarnNullableRExp
    { _warnPos  :: AlexPosn  -- ^ The position of the code following the regex.
    , _warnText :: String    -- ^ Warning text.
    }

data PState = PState
  { warnings  :: [Warning]           -- ^ Stack of warnings, top = last warning.
  , smac_env  :: Map String CharSet
  , rmac_env  :: Map String RExp
  }

newtype P' m a = P { unP :: StateT PState m a }
  deriving (Functor, Applicative, Alternative, Monad, MonadTrans, MonadPlus)

type P = P' PBase

-- | Run the parser on given input.
runP :: MonadBasicParse m
     => String
          -- ^ Input string.
     -> (Map String CharSet, Map String RExp)
          -- ^ Character set and regex definitions.
     -> P' m a
          -- ^ Parsing computation.
     -> Either ParseError ([Warning], a)
          -- ^ List of warnings in first-to-last order, result.
runP str (senv,renv) p = runPBase str $ do
  (a, s) <- runStateT (unP p) initial_state
  return (reverse (warnings s), a)
  where
  initial_state = PState
    { warnings  = []
    , smac_env  = senv
    , rmac_env  = renv
    }

raiseP :: MonadBasicParse m => ParseError -> P' m a
raiseP = P . lift . raisePBase

failP :: MonadBasicParse m => String -> P' m a
failP = P . lift . failPBase

-- Macros are expanded during parsing, to simplify the abstract
-- syntax.  The parsing monad passes around two environments mapping
-- macro names to sets and regexps respectively.

lookupSMac :: MonadBasicParse m => (AlexPosn,String) -> P' m CharSet
lookupSMac (posn, smac) = do
  PState{ smac_env = senv } <- P get
  case Map.lookup smac senv of
    Just ok -> return ok
    Nothing -> raiseP (Just posn, "unknown set macro: $" ++ smac)

lookupRMac :: MonadBasicParse m => String -> P' m RExp
lookupRMac rmac = do
  PState{ rmac_env = renv } <- P get
  case Map.lookup rmac renv of
    Just ok -> return ok
    Nothing -> raiseP (Nothing, "unknown regex macro: %" ++ rmac)

newSMac :: Monad m => String -> CharSet -> P' m ()
newSMac smac set
  = P $ modify $ \s -> s { smac_env = Map.insert smac set (smac_env s) }

newRMac :: Monad m => String -> RExp -> P' m ()
newRMac rmac rexp
  = P $ modify $ \s -> s { rmac_env = Map.insert rmac rexp (rmac_env s) }

-- | Add a warning if given regular expression is nullable
--   unless the user wrote the regex 'Eps'.
warnIfNullable
  :: Monad m
  => RExp       -- ^ Regular expression.
  -> AlexPosn   -- ^ Position associated to regular expression.
  -> P' m ()
-- If the user wrote @()@, they wanted to match the empty sequence!
-- Thus, skip the warning then.
warnIfNullable Eps _ = return ()
warnIfNullable r pos = P $
  when (nullable r) $ modify $ \ s -> s {
      warnings = WarnNullableRExp pos w : warnings s
    }
  where
  w = unwords
      [ "Regular expression"
      , show r
      , "matches the empty string."
      ]
