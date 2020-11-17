{
{-# LANGUAGE OverloadedStrings #-}
module Main (main) where
import System.Exit
import qualified Data.Text.Lazy as T
import Data.Text.Lazy.Encoding
}

%wrapper "basic-bytestring"

tokens :-

  ∃^∀    { const True }
  ∃      { const True }
  .      { const False }

{
main :: IO ()
main = if and . alexScanTokens . encodeUtf8 $ "∃∀"
           then exitWith ExitSuccess
           else exitFailure
}