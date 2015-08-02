{
{-# LANGUAGE FlexibleContexts #-}

module Main (main) where
import System.Exit
import Prelude hiding (lex)
import Control.Monad.State
import qualified Data.Bits
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
utf8Encode :: Char -> [Word8]
utf8Encode = map fromIntegral . go . ord
 where
  go oc
   | oc <= 0x7f       = [oc]

   | oc <= 0x7ff      = [ 0xc0 + (oc `Data.Bits.shiftR` 6)
                        , 0x80 + oc Data.Bits..&. 0x3f
                        ]

   | oc <= 0xffff     = [ 0xe0 + (oc `Data.Bits.shiftR` 12)
                        , 0x80 + ((oc `Data.Bits.shiftR` 6) Data.Bits..&. 0x3f)
                        , 0x80 + oc Data.Bits..&. 0x3f
                        ]
   | otherwise        = [ 0xf0 + (oc `Data.Bits.shiftR` 18)
                        , 0x80 + ((oc `Data.Bits.shiftR` 12) Data.Bits..&. 0x3f)
                        , 0x80 + ((oc `Data.Bits.shiftR` 6) Data.Bits..&. 0x3f)
                        , 0x80 + oc Data.Bits..&. 0x3f
                        ]

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
ignorePendingBytes (p,c,ps,s) = (p,c,[],s)

alexInputPrevChar :: AlexInput -> Char
alexInputPrevChar (p,c,bs,s) = c

alexGetByte :: AlexInput -> Maybe (Byte,AlexInput)
alexGetByte (p,c,(b:bs),s) = Just (b,(p,c,bs,s))
alexGetByte (p,c,[],[]) = Nothing
alexGetByte (p,_,[],(c:s))  = let p' = alexMove p c
                                  (b:bs) = utf8Encode c
                              in p' `seq`  Just (b, (p', c, bs, s))

data AlexPosn = AlexPn !Int !Int !Int
        deriving (Eq,Show)

alexStartPos :: AlexPosn
alexStartPos = AlexPn 0 1 1

alexMove :: AlexPosn -> Char -> AlexPosn
alexMove (AlexPn a l c) '\t' = AlexPn (a+1)  l     (((c+7) `div` 8)*8+1)
alexMove (AlexPn a l c) '\n' = AlexPn (a+1) (l+1)   1
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
    AlexSkip  inp' len -> do
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
  let
    (result, _) = runState lex AlexState { alex_pos = alexStartPos,
                                           alex_inp = input,
                                           alex_chr = '\n',
                                           alex_bytes = [],
                                           alex_scd = 0,
                                           alex_errs= [] }
  in do
    if result /= tokens
      then exitFailure
      else exitWith ExitSuccess

}
