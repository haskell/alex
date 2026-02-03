{
module WyvernLexerV2
  (main,
  alexScanTokens,
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

main = do
    putStrLn "Wyvern lexer v2 correct example: "
    correctFileContent <- readFile "./wyvern/correct-input.txt"
    let correctTokens = alexScanTokens correctFileContent
    print correctTokens

    putStrLn "Wyvern lexer v2 incorrect example: "
    incorrectFileContent <- readFile "./wyvern/incorrect-input.txt"
    let incorrectTokens = alexScanTokens incorrectFileContent
    print incorrectTokens
}
