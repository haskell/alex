{
module LexerV3
  (lexAll,
  runAlex,
  Token(TokenAction,
  TokenSoloIdentifier,
  TokenOCB,
  TokenCCB),
  AlexPosn(..)) where
}

%wrapper "monad"

$digit        = 0-9
$alpha        = [a-zA-Z]

$idChar       = [$alpha $digit \']
$contentChar  = [$alpha $digit $white \' \, \! \- \. \/ \? \= \< \> \[ \] \+ \( \)]

@id           = $idChar+
@content      = $contentChar+

tokens :-

  $white+                         ;
  @id [$white]+ \"@content\"      { (\(position, _previousCharacter, _bytes, inputString) len -> return $ TokenAction position (take len inputString)) }
  \"@content\"                    { (\(position, _previousCharacter, _bytes, inputString) len -> return $ TokenAction position ("# " <> take len inputString)) -- # is a placeholder id that will later be replaced by a unique identifier }
  @id                             { (\(position, _previousCharacter, _bytes, inputString) len -> return $ TokenSoloIdentifier position (take len inputString)) }
  \{                              { (\(position, _previousCharacter, _bytes, _inputString) len -> return $ TokenOCB position) }
  \}                              { (\(position, _previousCharacter, _bytes, _inputString) len -> return $ TokenCCB position) }

{
-- Each token action (the right hand side function) is of type :: AlexInput -> Int -> Alex Token

data Token
  = TokenAction AlexPosn String
  | TokenSoloIdentifier AlexPosn String
  | TokenOCB AlexPosn
  | TokenCCB AlexPosn
  | TokenEOF
  deriving (Eq, Show)

alexEOF :: Alex Token
alexEOF = return TokenEOF

lexAll :: Alex [Token]
lexAll = go
    where
        go = do
            t <- alexMonadScan
            case t of
                TokenEOF -> return []
                _ -> (t:) <$> go
}
