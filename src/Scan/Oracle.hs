module Scan.Oracle (lexToken) where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans.State
import Data.Char
import Data.Maybe (fromMaybe)

import ParseMonad
import ParseMonad.Oracle
import Token hiding (isIdChar)

-- -----------------------------------------------------------------------------
-- Character classes

isAsciiAlpha, isAsciiAlphaNum, isIdChar, isSpecial, isPrintable, isGraphic,
  isNonspecial :: Char -> Bool

isAsciiAlpha c = isAsciiUpper c || isAsciiLower c

isAsciiAlphaNum c = isAsciiAlpha c || isDigit c

isIdChar c = isAsciiAlphaNum c || c == '_' || c == '\''

isSpecial = (`elem` ".;,$|*+?#~-{}()[]^/")

-- Keep in sync with Main.hs's definition, which has the same FIXME.
-- FIXME: Look it up the unicode standard
isPrintable c = c >= chr 32 && c <= chr 0x10FFFF

isGraphic c = isPrintable c && not (isSpace c)

isNonspecial c = isGraphic c && not (isSpecial c || c == '%')

-- -----------------------------------------------------------------------------
-- Macros (Lexemes we don't turn into tokens)

macId :: PBase String
macId = (:) <$> matching isAlpha <*> many (matching isIdChar)

mkMacMac :: Char -> PBase String
mkMacMac initChar = do
  exactly initChar
  macId <|> do
    exactly '{'
    r <- macId
    exactly '}'
    return r

macSmac :: PBase String
macSmac = mkMacMac '$'

macRmac :: PBase String
macRmac = mkMacMac '@'

macComment :: PBase ()
macComment = () <$ do
  exactly '-'
  exactly '-'
  many (matching (/= '\n'))

macWhiteSpace :: PBase ()
macWhiteSpace = (() <$ some (matching isSpace)) <|> macComment


-- -----------------------------------------------------------------------------
-- Lexemes

lexemeWhiteSpace :: PBase () -- white space; ignore
lexemeWhiteSpace = macWhiteSpace

lexemeString :: PBase String
lexemeString = do
  exactly '"'
  res <- many $ matching (/= '"')
  exactly '"'
  return res

lexemeBind :: PBase (Maybe String)
lexemeBind = do
  name <- optional $ do
    name <- macId
    _ <- optional macWhiteSpace
    return name
  exactlyS ":-"
  return name

lexemeCode :: PBase ()
lexemeCode = do
  exactly '{'
  mc <- peekPBase
  case mc of
    Nothing -> empty
    Just c -> guard $ c == '\n' || not (isDigit c)

lexemeSpecial :: PBase Char -- note: matches {
lexemeSpecial = matching isSpecial

lexemeWrapper :: PBase ()
lexemeWrapper = do
  exactly '%'
  exactlyS "wrapper"

lexemeEncoding :: PBase ()
lexemeEncoding = do
  exactly '%'
  exactlyS "encoding"

lexemeAction :: PBase ()
lexemeAction = do
  exactly '%'
  exactlyS "action"

lexemeToken :: PBase ()
lexemeToken = do
  exactly '%'
  exactlyS "token"

lexemeTypeclass :: PBase ()
lexemeTypeclass = do
  exactly '%'
  exactlyS "typeclass"

lexemeDecch :: PBase String
lexemeDecch = do
  exactly '\\'
  some $ matching isDigit

lexemeHexch :: PBase String
lexemeHexch = do
  exactly '\\'
  exactly 'x'
  some $ matching isHexDigit

lexemeOctch :: PBase String
lexemeOctch = do
  exactly '\\'
  exactly 'x'
  some $ matching isOctDigit

lexemeEscape :: PBase Char
lexemeEscape = do
  exactly '\\'
  matching isPrintable

lexemeChar :: PBase Char -- includes 1 digit numbers
lexemeChar = matching $ \c -> isNonspecial c && c /= '<'

lexemeNum :: PBase String -- should be after char
lexemeNum = some $ matching isDigit

lexemeSmac :: PBase String
lexemeSmac = macSmac

lexemeRmac :: PBase String
lexemeRmac = macRmac

mkMacDef :: PBase String -> PBase String
mkMacDef macId' = do
  name <- macId'
  _ <- optional macWhiteSpace
  exactly '='
  return name

lexemeSmacDef :: PBase String
lexemeSmacDef = mkMacDef macSmac

lexemeRmacDef :: PBase String
lexemeRmacDef = mkMacDef macRmac

-- identifiers are allowed to be unquoted in startcode lists

lexemeStartCodeBegin :: PBase ()
lexemeStartCodeBegin = exactly '<'

lexemeStartCodeZero :: PBase ()
lexemeStartCodeZero = exactly '0'

lexemeStartCodeId :: PBase String
lexemeStartCodeId = macId

lexemeStartComma :: PBase ()
lexemeStartComma = exactly ','

lexemeStartCodeEnd :: PBase ()
lexemeStartCodeEnd = exactly '>'

-- After a <..> startcode sequence, we can have a {...} grouping of rules,
-- so don't try to interpret the opening { as a code block.
lexemeAfterStartCodes0 :: PBase Char
lexemeAfterStartCodes0 = do
  exactly '{'
  matching $ \c -> c == '\n' || not (isDigit c)

lexemeAfterStartCodes1 :: PBase ()  -- note: empty pattern
lexemeAfterStartCodes1 = return ()

-- -----------------------------------------------------------------------------
-- Putting it together

lexToken :: PBase Token
lexToken =
  (lexemeWhiteSpace >> lexToken) -- white space; ignore
  <|> do
    sc <- startCode <$> PBase get
    case sc of
      StartCode_0 -> wrap $
        (StringT <$> lexemeString) <|>
        (BindT . fromMaybe "" <$> lexemeBind) <|>
        (lexemeCode >> code) <|>
        (SpecialT <$> lexemeSpecial) <|>
        (WrapperT <$ lexemeWrapper) <|>
        (EncodingT <$ lexemeEncoding) <|>
        (ActionTypeT <$ lexemeAction) <|>
        (TokenTypeT <$ lexemeToken) <|>
        (TypeClassT <$ lexemeTypeclass) <|>

        (CharT . do_ech 10 <$> lexemeDecch) <|>
        (CharT . do_ech 16 <$> lexemeHexch) <|>
        (CharT . do_ech 8  <$> lexemeOctch) <|>
        (CharT . esc <$> lexemeEscape) <|>
        (CharT <$> lexemeChar) <|>
        (NumT . parseInt 10 <$> lexemeNum) <|>
        (SMacT <$> lexemeSmac) <|>
        (RMacT <$> lexemeRmac) <|>

        (SMacDefT <$> lexemeSmacDef) <|>
        (RMacDefT <$> lexemeRmacDef) <|>

        (SpecialT '<' <$ lexemeStartCodeBegin <* setStartCode StartCode_StartCodes)
      StartCode_StartCodes -> wrap $
        (ZeroT <$ lexemeStartCodeZero) <|>
        (IdT <$> lexemeStartCodeId) <|>
        (SpecialT ',' <$ lexemeStartComma) <|>
        (SpecialT '>' <$ lexemeStartCodeEnd <* setStartCode StartCode_AfterStartCodes)

      StartCode_AfterStartCodes ->
        (wrap (SpecialT <$> lexemeAfterStartCodes0) <* setStartCode StartCode_0) <|>
        (lexemeAfterStartCodes1 >> setStartCode StartCode_0 >> lexToken)
--lexToken = do
--  (p, mc) <- nextChar
--  case mc of
--    Nothing -> return $ T p EOFT
--    Just c -> case c of
--      -- during bootstrap, must be a comment
--      '-' -> do
--        (p, mc) <- nextChar
--        case mc of
--          Nothing -> lexError "Bootstrap lexer requires - after -, found EOF"
--          Just '-' -> lexError "asdf"
--          Just c -> lexError $ "Bootsetrap lexer requires - after -, found \''" ++ [c,'\'']

-- -----------------------------------------------------------------------------
-- Utilities

nextChar :: PBase Char
nextChar = do
  mc <- nextCharPBase
  case mc of
    Nothing -> lexError "unexpected EOF"
    Just c -> return c

exactly :: Char -> PBase ()
exactly c = () <$ matching (== c)

exactlyS :: String -> PBase ()
exactlyS = mapM_ exactly

matching :: (Char -> Bool) -> PBase Char
matching p = do
  c <- nextChar
  guard $ p c
  return c

wrap :: PBase Tkn -> PBase Token
wrap m = do
  p <- curPosPBase
  T p <$> m
