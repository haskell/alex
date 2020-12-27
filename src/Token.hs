-------------------------------------------------------------------------------
--                  ALEX TOKENS AND SCANNING ROUTINES
--
-- The Scanner module containss the scanner proper.
--
-- See the Alex manual for a discussion of the scanners defined here.
--
-- Chris Dornan, Aug-95, 4-Jun-96, 10-Jul-96, 29-Sep-97
-------------------------------------------------------------------------------

module Token where

import Data.Char
--import Debug.Trace

import ParseMonad

-- -----------------------------------------------------------------------------
-- Token type

data Token = T AlexPosn Tkn
  deriving Show

tokPosn :: Token -> AlexPosn
tokPosn (T p _) = p

data Tkn
  = SpecialT Char
  | CodeT String
  | ZeroT
  | IdT String
  | StringT String
  | BindT String
  | CharT Char
  | SMacT String
  | RMacT String
  | SMacDefT String
  | RMacDefT String
  | NumT Int
  | WrapperT
  | EncodingT
  | ActionTypeT
  | TokenTypeT
  | TypeClassT
  | EOFT
  deriving Show

-- -----------------------------------------------------------------------------
-- Token functions

isIdChar :: Char -> Bool
isIdChar c = isAlphaNum c || c `elem` "_'"

extract :: Int -> String -> String
extract ln str = take (ln-2) (tail str)

do_ech :: Int -> String -> Char
do_ech radix str = chr (parseInt radix str)

mac :: Int -> String -> String
mac ln str = take (ln-1) $ tail str

-- TODO : replace not . isSpace with (\c -> not (isSpace c) && c /= '=')
macdef :: String -> String
macdef str = takeWhile (\c -> not (isSpace c) && c /= '=') $ tail str

esc :: Char -> Char
esc c =
  case c of
    'a' -> '\a'
    'b' -> '\b'
    'f' -> '\f'
    'n' -> '\n'
    'r' -> '\r'
    't' -> '\t'
    'v' -> '\v'
    _   ->  c

parseInt :: Int -> String -> Int
parseInt radix ds = foldl1 (\n d -> n * radix + d) (map digitToInt ds)

-- In brace-delimited code, we have to be careful to match braces
-- within the code, but ignore braces inside strings and character
-- literals.  We do an approximate job (doing it properly requires
-- implementing a large chunk of the Haskell lexical syntax).

code :: PBase Tkn
code = go 1 ""
  where
    go :: Int -> String -> PBase Tkn
    go 0 cs = do
      return $ CodeT $ triml $ reverse $ triml $ tail cs
    go n cs = do
      c <- next
      case c of
        '{'  -> go (n+1) (c:cs)
        '}'  -> go (n-1) (c:cs)
        '\'' -> go_char n (c:cs)
        '\"' -> go_str n (c:cs) '\"'
        c2   -> go n (c2:cs)

    go_char :: Int -> String -> PBase Tkn
    -- try to catch multiple occurrences of ' at identifier end
    go_char n cs@('\'':'\'':_) = go n cs
    -- try to catch occurrences of ' within an identifier
    go_char n cs@('\'':c2:_)
      | isAlphaNum c2              = go n cs
    go_char n cs               = go_str n cs '\''

    go_str :: Int -> String -> Char -> PBase Tkn
    go_str n cs end = do
      c <- next
      if c == end
        then go n (c:cs)
        else case c of
          '\\' -> do
            d <- next
            go_str n (d:c:cs) end
          _   -> go_str n (c:cs) end

    next :: PBase Char
    next = do
      r <- nextCharPBase
      case r of
        Nothing -> lexError "lexical error in code fragment"
        Just c  -> return c

    triml = dropWhile isSpace


lexError :: String -> PBase a
lexError s = do
  mc <- peekPBase
  failPBase $ s ++ case mc of
    Nothing -> " at end of file"
    Just c ->" at " ++ show c
