{

module Main (main) where
import System.Exit
import Prelude hiding (lex)

}

%wrapper "gscan"
%token "[Token s]"
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

tokpred :: () -> AlexInput -> Int -> AlexInput -> Bool
tokpred _ _ _ _ = True

idtoken :: Read s => Int -> AlexPosn -> Char -> String -> Int ->
                     ((Int,state) -> [Token s]) -> (Int,state) -> [Token s]
idtoken n _ _ s len cont st = Id n (read ("\"" ++ take len s ++ "\"")) : cont st

data Token s = Id Int s deriving Eq

lex :: Read s => String -> [Token s]
lex str = alexGScan (\_ _ _ _ -> []) (0 :: Int) str

input = "abab\ndddc.fff\ngh\nijji.\nllmnm\noop.rq0tsst\n3uuvu.5xxw"

tokens = [ Id 0 "abab", Id 1 "dddc", Id 2 "fff", Id 3 "gh", Id 4 "ijji",
           Id 5 "ll", Id 6 "mnm", Id 7 "oop", Id 8 "rq", Id 9 "tsst",
           Id 10 "uuvu", Id 11 "xxw"]

main :: IO ()
main =
  let
    result :: [Token String]
    result = lex input
  in do
    if result /= tokens
      then exitFailure
      else exitWith ExitSuccess

}
