{
module LexerV2
  (alexScanTokens,
  Token(TokenAction,
  TokenSoloIdentifier,
  TokenOCB,
  TokenCCB),
  AlexPosn(..)) where
}

%wrapper "posn"

$digit        = 0-9
$alpha        = [a-zA-Z]

$idChar       = [$alpha $digit \']
$contentChar  = [$alpha $digit $white \' \, \! \- \. \/ \? \= \< \> \[ \] \+ \( \)]

@id           = $idChar+
@content      = $contentChar+

tokens :-

  $white+                         ;
  @id [$white]+ \"@content\"      { (\position input -> TokenAction position input) }
  \"@content\"                    { (\position input -> TokenAction position ("# " <> input)) -- # is a placeholder id that will later be replaced by a unique identifier }
  @id                             { (\position input -> TokenSoloIdentifier position input) }
  \{                              { (\position _ -> TokenOCB position) }
  \}                              { (\position _ -> TokenCCB position) }

{
-- Each token action (the right hand side function) is of type :: AlexPosn -> String -> Token

data Token
  = TokenAction AlexPosn String
  | TokenSoloIdentifier AlexPosn String
  | TokenOCB AlexPosn
  | TokenCCB AlexPosn
  deriving (Eq, Show)
}
