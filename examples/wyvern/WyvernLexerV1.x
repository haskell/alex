{
module WyvernLexerV1
  (main,
  alexScanTokens,
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

main = do
    putStrLn "Wyvern lexer v1 correct example: "
    correctFileContent <- readFile "./wyvern/correct-input.txt"
    let correctTokens = alexScanTokens correctFileContent
    print correctTokens

    putStrLn "Wyvern lexer v1 incorrect example: "
    incorrectFileContent <- readFile "./wyvern/incorrect-input.txt"
    let incorrectTokens = alexScanTokens incorrectFileContent
    print incorrectTokens
}
