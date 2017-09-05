{
{-# LANGUAGE OverloadedStrings #-}
module Main (main) where
import System.Exit
import qualified Data.ByteString.Lazy.Char8 as B
}

%wrapper "monadUserState-bytestring"
%encoding "iso-8859-1"

$digit = 0-9			-- digits
$alpha = [a-zA-Z]		-- alphabetic characters

tokens :-

  $white+				;
  "--".*				;
  let					{ tok (\p _ -> Let p) }
  in					{ tok (\p _ -> In p) }
  $digit+                               { tok (\p s -> Int p (read (B.unpack s))) }
  [\=\+\-\*\/\(\)]                      { tok (\p s -> Sym p (head (B.unpack s))) }
  $alpha [$alpha $digit \_ \']*         { tok (\p s -> Var p (B.unpack s)) }

{
-- Each right-hand side has type :: AlexPosn -> String -> Token

-- Some action helpers:
tok f (p,_,input,_) len = return (f p (B.take (fromIntegral len) input))

-- The token type:
data Token =
	Let AlexPosn		|
	In  AlexPosn		|
	Sym AlexPosn Char	|
        Var AlexPosn String     |
	Int AlexPosn Int	|
        Err AlexPosn            |
        EOF
        deriving (Eq,Show)

alexEOF = return EOF

main = if test1 /= result1 then do print test1; exitFailure
			   else exitWith ExitSuccess

type AlexUserState = ()
alexInitUserState = ()

scanner str = runAlex str $ do
  let loop = do tk <- alexMonadScan
                if tk == EOF
                        then return [tk]
			else do toks <- loop
                                return (tk:toks)
  loop

test1 = case scanner "  let in 012334\n=+*foo bar__'" of
          Left err -> error err
          Right toks -> toks

result1 = [Let (AlexPn 2 1 3),In (AlexPn 6 1 7),Int (AlexPn 9 1 10) 12334,Sym (AlexPn 16 2 1) '=',Sym (AlexPn 17 2 2) '+',Sym (AlexPn 18 2 3) '*',Var (AlexPn 19 2 4) "foo",Var (AlexPn 23 2 8) "bar__'", EOF]


}
