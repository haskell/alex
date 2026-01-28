{
module LexerV1
  (alexScanTokens,
  Token(TokenAction,
  TokenSoloIdentifier,
  TokenOCB,
  TokenCCB)) where
}

%wrapper "basic"

$digit        = 0-9
$alpha        = [a-zA-Z]

$idChar       = [$alpha $digit \']
$contentChar  = [$alpha $digit $white \' \, \! \- \. \/ \? \= \< \> \[ \] \+ \( \)]

@id           = $idChar+
@content      = $contentChar+

tokens :-

  $white+                         ;
  @id [$white]+ \"@content\"      { \s -> TokenAction s }
  \"@content\"                    { \s -> TokenAction ("# " <> s) -- # is a placeholder id that will later be replaced by a unique identifier }
  @id                             { \s -> TokenSoloIdentifier s }
  \{                              { \_ -> TokenOCB }
  \}                              { \_ -> TokenCCB }

{
data Token
  = TokenAction String
  | TokenSoloIdentifier String
  | TokenOCB
  | TokenCCB
  deriving Show
}
