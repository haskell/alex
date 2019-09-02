{
{-# LANGUAGE FlexibleContexts, MultiParamTypeClasses, FunctionalDependencies,
             FlexibleInstances #-}

module Main (main) where
import System.Exit
import Prelude hiding (lex)
import qualified Data.Bits
import Control.Applicative
import Control.Monad
import Data.Word
import Data.Char

}

%action "AlexInput -> Int -> m (Token s)"
%typeclass "Read s, MonadState AlexState m"

tokens :-

[a-b]+$                     { idtoken 0 }
[c-d]+/"."                  { idtoken 1 }
[e-f]+/{ tokpred }          { idtoken 2 }
^[g-h]+$                    { idtoken 3 }
^[i-j]+/"."                 { idtoken 4 }
^[k-l]+/{ tokpred }         { idtoken 5 }
[m-n]+$                     { idtoken 6 }
[o-p]+/"."                  { idtoken 7 }
[q-r]+/{ tokpred }          { idtoken 8 }
[0-1]^[s-t]+$               { idtoken 9 }
[2-3]^[u-v]+/"."            { idtoken 10 }
[4-5]^[w-x]+/{ tokpred }    { idtoken 11 }
[y-z]+                      { idtoken 12 }
[A-B]+$                     ;
[C-D]+/"."                  ;
[E-F]+/{ tokpred }          ;
^[G-H]+$                    ;
^[I-J]+/"."                 ;
^[K-L]+/{ tokpred }         ;
[M-N]+$                     ;
[O-P]+/"."                  ;
[Q-R]+/{ tokpred }          ;
[0-1]^[S-T]+$               ;
[2-3]^[U-V]+/"."            ;
[4-5]^[W-X]+/{ tokpred }    ;
[Y-Z]+                      ;
\.                          ;
[ \n\t\r]+                  ;
[0-9]                       ;

{

-- | Encode a Haskell String to a list of Word8 values, in UTF8 format.
utf8Encode' :: Char -> (Word8, [Word8])
utf8Encode' c = case go (ord c) of
                  (x, xs) -> (fromIntegral x, map fromIntegral xs)
 where
  go oc
   | oc <= 0x7f       = ( oc
                        , [
                        ])

   | oc <= 0x7ff      = ( 0xc0 + (oc `Data.Bits.shiftR` 6)
                        , [0x80 + oc Data.Bits..&. 0x3f
                        ])

   | oc <= 0xffff     = ( 0xe0 + (oc `Data.Bits.shiftR` 12)
                        , [0x80 + ((oc `Data.Bits.shiftR` 6) Data.Bits..&. 0x3f)
                        , 0x80 + oc Data.Bits..&. 0x3f
                        ])
   | otherwise        = ( 0xf0 + (oc `Data.Bits.shiftR` 18)
                        , [0x80 + ((oc `Data.Bits.shiftR` 12) Data.Bits..&. 0x3f)
                        , 0x80 + ((oc `Data.Bits.shiftR` 6) Data.Bits..&. 0x3f)
                        , 0x80 + oc Data.Bits..&. 0x3f
                        ])

type Byte = Word8

data AlexState = AlexState {
        alex_pos :: !AlexPosn,  -- position at current input location
        alex_inp :: String,     -- the current input
        alex_chr :: !Char,      -- the character before the input
        alex_bytes :: [Byte],
        alex_scd :: !Int,        -- the current startcode
        alex_errs :: [String]


    }

type AlexInput = (AlexPosn,     -- current position,
                  Char,         -- previous char
                  [Byte],       -- pending bytes on current char
                  String)       -- current input string

ignorePendingBytes :: AlexInput -> AlexInput
ignorePendingBytes (p,c,_,s) = (p,c,[],s)

alexInputPrevChar :: AlexInput -> Char
alexInputPrevChar (_,c,_,_) = c

alexGetByte :: AlexInput -> Maybe (Byte,AlexInput)
alexGetByte (p,c,(b:bs),s) = Just (b,(p,c,bs,s))
alexGetByte (_,_,[],[]) = Nothing
alexGetByte (p,_,[],(c:s))  = let p' = alexMove p c
                              in case utf8Encode' c of
                                   (b, bs) -> p' `seq`  Just (b, (p', c, bs, s))

data AlexPosn = AlexPn !Int !Int !Int
        deriving (Eq,Show)

alexStartPos :: AlexPosn
alexStartPos = AlexPn 0 1 1

alexMove :: AlexPosn -> Char -> AlexPosn
alexMove (AlexPn a l c) '\t' = AlexPn (a+1)  l     (((c+7) `div` 8)*8+1)
alexMove (AlexPn a l _) '\n' = AlexPn (a+1) (l+1)   1
alexMove (AlexPn a l c) _    = AlexPn (a+1)  l     (c+1)

alexGetInput :: MonadState AlexState m => m AlexInput
alexGetInput =
  do
    AlexState { alex_pos = pos, alex_chr = c,
                alex_bytes = bs, alex_inp = inp } <- get
    return (pos, c, bs, inp)

alexSetInput :: MonadState AlexState m => AlexInput -> m ()
alexSetInput (pos, c, bs, inp) =
  do
    s <- get
    put s { alex_pos = pos, alex_chr = c,
            alex_bytes = bs, alex_inp = inp }

alexError :: (MonadState AlexState m, Read s) => String -> m (Token s)
alexError message =
  do
    s @ AlexState { alex_errs = errs } <- get
    put s { alex_errs = message : errs }
    alexMonadScan

alexGetStartCode :: MonadState AlexState m => m Int
alexGetStartCode =
  do
    AlexState{ alex_scd = sc } <- get
    return sc

alexSetStartCode :: MonadState AlexState m => Int -> m ()
alexSetStartCode sc =
  do
    s <- get
    put s { alex_scd = sc }

alexMonadScan :: (MonadState AlexState m, Read s) => m (Token s)
alexMonadScan = do
  inp <- alexGetInput
  sc <- alexGetStartCode
  case alexScan inp sc of
    AlexEOF -> alexEOF
    AlexError ((AlexPn _ line column),_,_,_) ->
      alexError $ "lexical error at line " ++ (show line) ++
                  ", column " ++ (show column)
    AlexSkip  inp' _ -> do
        alexSetInput inp'
        alexMonadScan
    AlexToken inp' len action -> do
        alexSetInput inp'
        action (ignorePendingBytes inp) len

alexEOF :: MonadState AlexState m => m (Token s)
alexEOF = return EOF

tokpred :: () -> AlexInput -> Int -> AlexInput -> Bool
tokpred _ _ _ _ = True

idtoken :: (Read s, MonadState AlexState m) =>
           Int -> AlexInput -> Int -> m (Token s)
idtoken n (_, _, _, s) len = return (Id n (read ("\"" ++ take len s ++ "\"")))

data Token s = Id Int s | EOF deriving Eq

lex :: (MonadState AlexState m, Read s) => m [Token s]
lex =
  do
    res <- alexMonadScan
    case res of
      EOF -> return []
      tok ->
        do
          rest <- lex
          return (tok : rest)

input = "abab\ndddc.fff\ngh\nijji.\nllmnm\noop.rq0tsst\n3uuvu.5xxw"

tokens = [ Id 0 "abab", Id 1 "dddc", Id 2 "fff", Id 3 "gh", Id 4 "ijji",
           Id 5 "ll", Id 6 "mnm", Id 7 "oop", Id 8 "rq", Id 9 "tsst",
           Id 10 "uuvu", Id 11 "xxw"]

main :: IO ()
main =
  do
    (result, _) <- runStateT lex AlexState { alex_pos = alexStartPos,
                                             alex_inp = input,
                                             alex_chr = '\n',
                                             alex_bytes = [],
                                             alex_scd = 0,
                                             alex_errs= [] }
    if result /= tokens
      then exitFailure
      else exitWith ExitSuccess

-- | Minimal definition is either both of @get@ and @put@ or just @state@
class Monad m => MonadState s m | m -> s where
    -- | Return the state from the internals of the monad.
    get :: m s
    get = state (\s -> (s, s))

    -- | Replace the state inside the monad.
    put :: s -> m ()
    put s = state (\_ -> ((), s))

    -- | Embed a simple state action into the monad.
    state :: (s -> (a, s)) -> m a
    state f = do
      s <- get
      let ~(a, s') = f s
      put s'
      return a

-- | Construct a state monad computation from a function.
-- (The inverse of 'runState'.)
state' :: Monad m
       => (s -> (a, s))  -- ^pure state transformer
       -> StateT s m a   -- ^equivalent state-passing computation
state' f = StateT (return . f)

-- ---------------------------------------------------------------------------
-- | A state transformer monad parameterized by:
--
--   * @s@ - The state.
--
--   * @m@ - The inner monad.
--
-- The 'return' function leaves the state unchanged, while @>>=@ uses
-- the final state of the first computation as the initial state of
-- the second.
newtype StateT s m a = StateT { runStateT :: s -> m (a,s) }

-- | Evaluate a state computation with the given initial state
-- and return the final value, discarding the final state.
--
-- * @'evalStateT' m s = 'liftM' 'fst' ('runStateT' m s)@
evalStateT :: (Monad m) => StateT s m a -> s -> m a
evalStateT m s = do
    (a, _) <- runStateT m s
    return a

-- | Evaluate a state computation with the given initial state
-- and return the final state, discarding the final value.
--
-- * @'execStateT' m s = 'liftM' 'snd' ('runStateT' m s)@
execStateT :: (Monad m) => StateT s m a -> s -> m s
execStateT m s = do
    (_, s') <- runStateT m s
    return s'

-- | Map both the return value and final state of a computation using
-- the given function.
--
-- * @'runStateT' ('mapStateT' f m) = f . 'runStateT' m@
mapStateT :: (m (a, s) -> n (b, s)) -> StateT s m a -> StateT s n b
mapStateT f m = StateT $ f . runStateT m

-- | @'withStateT' f m@ executes action @m@ on a state modified by
-- applying @f@.
--
-- * @'withStateT' f m = 'modify' f >> m@
withStateT :: (s -> s) -> StateT s m a -> StateT s m a
withStateT f m = StateT $ runStateT m . f

instance (Functor m) => Functor (StateT s m) where
    fmap f m = StateT $ \ s ->
        fmap (\ (a, s') -> (f a, s')) $ runStateT m s

instance (Monad m) => Monad (StateT s m) where
    return a = state $ \s -> (a, s)
    m >>= k  = StateT $ \s -> do
        (a, s') <- runStateT m s
        runStateT (k a) s'

-- | Fetch the current value of the state within the monad.
get' :: (Monad m) => StateT s m s
get' = state $ \s -> (s, s)

-- | @'put' s@ sets the state within the monad to @s@.
put' :: (Monad m) => s -> StateT s m ()
put' s = state $ \_ -> ((), s)

-- | @'modify' f@ is an action that updates the state to the result of
-- applying @f@ to the current state.
--
-- * @'modify' f = 'get' >>= ('put' . f)@
modify' :: (Monad m) => (s -> s) -> StateT s m ()
modify' f = state $ \s -> ((), f s)

instance Monad m => MonadState s (StateT s m) where
    get = get'
    put = put'
    state = state'

instance (Functor m, Monad m) => Applicative (StateT s m) where
    pure = return
    (<*>) = ap

}
