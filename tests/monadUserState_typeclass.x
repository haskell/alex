{

module Main (main) where
import System.Exit
import Prelude hiding (lex)

}

%wrapper "monadUserState"
%token "Token s"
%typeclass "Read s"

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

type AlexUserState = Int

alexInitUserState = 0

alexEOF :: Alex (Token s)
alexEOF = return EOF

tokpred :: AlexUserState -> AlexInput -> Int -> AlexInput -> Bool
tokpred _ _ _ _ = True

idtoken :: Read s => Int -> AlexInput -> Int -> Alex (Token s)
idtoken n (_, _, _, s) len = return (Id n (read ("\"" ++ take len s ++ "\"")))

data Token s = Id Int s | EOF deriving Eq

lex :: Read s => String -> Either String [Token s]
lex inp =
  let
    lexAll =
      do
        res <- alexMonadScan
        case res of
          EOF -> return []
          tok ->
            do
              rest <- lexAll
              return (tok : rest)
  in
    runAlex inp lexAll

input = "abab\ndddc.fff\ngh\nijji.\nllmnm\noop.rq0tsst\n3uuvu.5xxw"

tokens = [ Id 0 "abab", Id 1 "dddc", Id 2 "fff", Id 3 "gh", Id 4 "ijji",
           Id 5 "ll", Id 6 "mnm", Id 7 "oop", Id 8 "rq", Id 9 "tsst",
           Id 10 "uuvu", Id 11 "xxw"]

main :: IO ()
main =
  let
    result = lex input
  in do
    case result of
      Left _ -> exitFailure
      Right toks ->
        do
          if toks /= tokens
            then exitFailure
            else exitWith ExitSuccess

}
