{
module Main (main) where
import {- "containers" -} Data.Set (Set)
import {- "containers" -} qualified Data.Set as Set
import {- "text" -} Data.Text (Text)
import {- "text" -} qualified Data.Text as Text
import {- "text" -} qualified Data.Text.Read as Text
import {- "base" -} Control.Arrow hiding (arr)
import {- "base" -} Control.Monad (forM_, when)
import {- "base" -} Control.Monad.Fail (MonadFail)
import {- "base" -} qualified Control.Monad.Fail as Fail (MonadFail (..))
import {- "base" -} Numeric.Natural
import {- "base" -} System.Exit
}

%wrapper "monadUserState-strict-text"
%token "Token integer"
%typeclass "Integral integer, Read integer, Show integer"

-- ugh
$digit = 0-9
$unidigit = 1-9
@number = [0] | $unidigit $digit*

tokens :-
  $white+ { skip }
  @number { \(_, _, _, s) len -> case Text.decimal (Text.take len s) of
                  Left e -> Fail.fail e
                  Right (n, txt)
                    | Text.null txt -> pure $ TokenInt n
                    | otherwise -> Fail.fail "got incomplete prefix " }
  [a-z]+  { \(_, _, _, s) len -> do
                let name = Text.take len s
                alexSeenVar name
                pure $ TokenVar name }
  [\+]    { mk0ary TokenAdd }
  [\-]    { mk0ary TokenSub }
  [\*]    { mk0ary TokenMul }
  [\/]    { mk0ary TokenDiv }
  [\^]    { mk0ary TokenPow }
  [\(]    { mk0ary TokenLPar }
  [\)]    { mk0ary TokenRPar }

{
mk0ary :: (Read integer, Integral integer) => Token integer -> AlexInput -> Int -> Alex (Token integer)
mk0ary tok _ _ = pure tok

data AlexUserState
  = AlexUserState {
      ausVars :: Set Text
  } deriving (Eq, Read, Show)

alexSeenVar :: Text -> Alex ()
alexSeenVar txt = do
  AlexUserState { ausVars = set } <- alexGetUserState
  alexSetUserState $ AlexUserState { ausVars = txt `Set.insert` set }

alexInitUserState :: AlexUserState
alexInitUserState = AlexUserState { ausVars = Set.empty }

data Token integer
  = TokenInt integer
  | TokenVar Text
  | TokenLPar
  | TokenRPar
  | TokenPow
  | TokenDiv
  | TokenMul
  | TokenSub
  | TokenAdd
  | EOF
  deriving (Eq, Read, Show)

alexEOF :: (Read integer, Integral integer) => Alex (Token integer)
alexEOF = pure EOF

instance MonadFail Alex where
  fail s = Alex . const $ Left s

evalAlex :: Text -> Alex t -> Either String (AlexUserState, t)
evalAlex txt alex = right (first getUserState) $ f state where
  f = unAlex alex
  getUserState AlexState { alex_ust = userState } = userState
  state = AlexState
    { alex_bytes = []
    , alex_pos = alexStartPos
    , alex_inp = txt
    , alex_chr = '\n'
    , alex_ust = alexInitUserState
    , alex_scd = 0 }

scanAll :: (Eq integer, Integral integer, Read integer, Show integer) => Alex [Token integer]
scanAll = alexMonadScan >>= \result -> case result of
  EOF -> pure []
  tok -> (tok :) <$> scanAll

tests :: [(Text, Set Text, [Token Natural])]
tests = [ (Text.pack "x*y/(x^3+y^3)"
          , Set.fromList [x, y]
          , [TokenVar x, TokenMul, TokenVar y, TokenDiv, TokenLPar, TokenVar x, TokenPow, TokenInt 3, TokenAdd, TokenVar y, TokenPow, TokenInt 3, TokenRPar])] where
  x = Text.pack "x"
  y = Text.pack "y"

main :: IO ()
main = do
  forM_ tests $ \(txt, vars, toks) -> do
    case evalAlex txt scanAll of
      Right (AlexUserState { ausVars = tokVars }, tokList)
        | tokVars == vars && toks == tokList -> pure ()
        | otherwise -> do
            when (toks /= tokList) $ do
              putStrLn $ "got " <> show tokList
              putStrLn $ "wanted " <> show toks
            when (tokVars /= vars) $ do
              putStrLn $ "got " <> show tokVars
              putStrLn $ "wanted " <> show vars
            exitFailure
      Left errorString -> do
        putStrLn $ "got error " <> errorString
        exitFailure
  exitSuccess
}
