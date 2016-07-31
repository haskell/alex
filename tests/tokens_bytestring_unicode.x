{
{-# LANGUAGE OverloadedStrings #-}
module Main (main) where
import System.Exit
import Data.ByteString.Lazy.Char8 (unpack)
}

%wrapper "basic-bytestring"
%encoding "utf-8"

$digit = 0-9      -- digits
$alpha = [a-zA-Zαβ]    -- alphabetic characters

tokens :-

  $white+        ;
  "--".*         ;
  let            { \_ -> Let }
  in             { \_ -> In }
  $digit+                               { \s -> Int (read (unpack s)) }
  [\=\+\-\*\/\(\)]                      { \s -> Sym (head (unpack s)) }
  $alpha [$alpha $digit \_ \']*         { \s -> Var (unpack s) }

{
-- Each right-hand side has type :: ByteString -> Token

-- The token type:
data Token =
  Let     |
  In      |
  Sym Char  |
  Var String  |
  Int Int    |
  Err
  deriving (Eq,Show)

main = if test1 /= result1 then exitFailure
                           else exitWith ExitSuccess

-- \206\177\206\178\206\178 is "αββ" utf-8 encoded
test1 = alexScanTokens "  let in 012334\n=+*foo \206\177\206\178\206\178 bar__'"
result1 = [Let,In,Int 12334,Sym '=',Sym '+',Sym '*',Var "foo",Var "\206\177\206\178\206\178",Var "bar__'"]
}
