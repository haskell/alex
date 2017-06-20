{
module Main (main) where
import System.Exit
}

%wrapper "basic" -- Defines: AlexInput, alexGetByte, alexPrevChar

$digit = 0-9
$alpha = [a-zA-Z]
$ws    = [\ \t\n]

tokens :-

  5 / {\ u _ibt _l _iat -> u == FiveIsMagic} { \s -> TFive (head s) }
  $digit { \s -> TDigit (head s) }
  $alpha { \s -> TAlpha (head s) }
  $ws    { \s -> TWSpace (head s) }

{

data Token = TDigit Char
           | TAlpha Char
           | TWSpace Char
           | TFive Char -- Predicated only
           | TLexError
    deriving (Eq,Show)

data UserLexerMode = NormalMode
                   | FiveIsMagic
    deriving Eq

main | test1 /= result1 = exitFailure
     | test2 /= result2 = exitFailure
     -- all succeeded
     | otherwise        = exitWith ExitSuccess

run_lexer :: UserLexerMode -> String -> [Token]
run_lexer m s = go ('\n', [], s)
    where go i@(_,_,s') = case alexScanUser m i 0 of
                     AlexEOF             -> []
                     AlexError  _i       -> [TLexError]
                     AlexSkip   i' _len  ->                   go i'
                     AlexToken  i' len t -> t (take len s') : go i'

test1 = run_lexer FiveIsMagic "5 x"
result1 = [TFive '5',TWSpace ' ',TAlpha 'x']

test2 = run_lexer NormalMode "5 x"
result2 = [TDigit '5',TWSpace ' ',TAlpha 'x']
}
