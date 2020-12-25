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

special, zero, string, bind, escape, decch, hexch, octch, char :: Action
num, smac, rmac, smacdef, rmacdef, startcode, wrapper, encoding :: Action
actionty, tokenty, typeclass :: Action
special   (p,_,str) _  = return $ T p (SpecialT  (head str))
zero      (p,_,_)   _  = return $ T p ZeroT
string    (p,_,str) ln = return $ T p (StringT (extract ln str))
bind      (p,_,str) _  = return $ T p (BindT (takeWhile isIdChar str))
escape    (p,_,str) _  = return $ T p (CharT (esc str))
decch     (p,_,str) ln = return $ T p (CharT (do_ech 10 ln (take (ln-1) (tail str))))
hexch     (p,_,str) ln = return $ T p (CharT (do_ech 16 ln (take (ln-2) (drop 2 str))))
octch     (p,_,str) ln = return $ T p (CharT (do_ech 8  ln (take (ln-2) (drop 2 str))))
char      (p,_,str) _  = return $ T p (CharT (head str))
num       (p,_,str) ln = return $ T p $ NumT $ parseInt 10 $ take ln str
smac      (p,_,str) ln = return $ T p (SMacT (mac ln str))
rmac      (p,_,str) ln = return $ T p (RMacT (mac ln str))
smacdef   (p,_,str) ln = return $ T p (SMacDefT (macdef ln str))
rmacdef   (p,_,str) ln = return $ T p (RMacDefT (macdef ln str))
startcode (p,_,str) ln = return $ T p (IdT (take ln str))
wrapper   (p,_,_)   _  = return $ T p WrapperT
encoding  (p,_,_)   _  = return $ T p EncodingT
actionty  (p,_,_)   _  = return $ T p ActionTypeT
tokenty   (p,_,_)   _  = return $ T p TokenTypeT
typeclass (p,_,_)   _  = return $ T p TypeClassT

isIdChar :: Char -> Bool
isIdChar c = isAlphaNum c || c `elem` "_'"

extract :: Int -> String -> String
extract ln str = take (ln-2) (tail str)

do_ech :: Int -> Int -> String -> Char
do_ech radix _ln str = chr (parseInt radix str)

mac :: Int -> String -> String
mac ln str = take (ln-1) $ tail str

-- TODO : replace not . isSpace with (\c -> not (isSpace c) && c /= '=')
macdef :: Int -> String -> String
macdef _ln str = takeWhile (\c -> not (isSpace c) && c /= '=') $ tail str

esc :: String -> Char
esc str =
  case head $ tail str of
    'a' -> '\a'
    'b' -> '\b'
    'f' -> '\f'
    'n' -> '\n'
    'r' -> '\r'
    't' -> '\t'
    'v' -> '\v'
    c   ->  c

parseInt :: Int -> String -> Int
parseInt radix ds = foldl1 (\n d -> n * radix + d) (map digitToInt ds)

-- In brace-delimited code, we have to be careful to match braces
-- within the code, but ignore braces inside strings and character
-- literals.  We do an approximate job (doing it properly requires
-- implementing a large chunk of the Haskell lexical syntax).

code :: Action
code (p,_,_inp) _ = do
  currentInput <- getInput
  go currentInput 1 ""
  where
    go :: AlexInput -> Int -> String -> P Token
    go inp 0 cs = do
      setInput inp
      return $ T p $ CodeT $ triml $ reverse $ triml $ tail cs
    go inp n cs = do
      case alexGetChar inp of
        Nothing       -> err inp
        Just (c,inp2) ->
          case c of
            '{'  -> go inp2 (n+1) (c:cs)
            '}'  -> go inp2 (n-1) (c:cs)
            '\'' -> go_char inp2 n (c:cs)
            '\"' -> go_str inp2 n (c:cs) '\"'
            c2   -> go inp2 n (c2:cs)

    go_char :: AlexInput -> Int -> String -> P Token
    -- try to catch multiple occurrences of ' at identifier end
    go_char inp n cs@('\'':'\'':_) = go inp n cs
    -- try to catch occurrences of ' within an identifier
    go_char inp n cs@('\'':c2:_)
      | isAlphaNum c2              = go inp n cs
    go_char inp n cs               = go_str inp n cs '\''

    go_str :: AlexInput -> Int -> String -> Char -> P Token
    go_str inp n cs end = do
      case alexGetChar inp of
          Nothing -> err inp
          Just (c,inp2)
            | c == end  -> go inp2 n (c:cs)
            | otherwise ->
              case c of
                '\\' -> case alexGetChar inp2 of
                          Nothing       -> err inp2
                          Just (d,inp3) -> go_str inp3 n (d:c:cs) end
                c2   -> go_str inp2 n (c2:cs) end

    err inp = do setInput inp; lexError "lexical error in code fragment"

    triml = dropWhile isSpace

lexError :: String -> P a
lexError s = do
  (_,_,_,input) <- getInput
  failP (s ++ (if (not (null input))
                  then " at " ++ show (head input)
                  else " at end of file"))

type Action = (AlexPosn,Char,String) -> Int -> P Token

andBegin :: Action -> StartCode -> Action
andBegin act sc inp len = setStartCode sc >> act inp len
