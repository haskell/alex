-- -----------------------------------------------------------------------------
--
-- Util.hs, part of Alex
--
-- (c) Simon Marlow 2003
--
-- General utilities used in various parts of Alex
--
-- ----------------------------------------------------------------------------}

module Util
  ( str
  , char
  , nl
  , paren
  , brack
  , interleave_shows
  , space
  , cjustify
  , ljustify
  , rjustify
  , spaces
  , hline
  ) where

-- Pretty-printing utilities

str :: String -> String -> String
str = showString

char :: Char -> String -> String
char c = (c :)

nl :: String -> String
nl = char '\n'

paren :: (String -> String) -> String -> String
paren s = char '(' . s . char ')'

brack :: (String -> String) -> String -> String
brack s = char '[' . s . char ']'

interleave_shows :: (String -> String) -> [String -> String] -> String -> String
interleave_shows _ [] = id
interleave_shows s xs = foldr1 (\a b -> a . s . b) xs

space :: String -> String
space = char ' '

cjustify, ljustify, rjustify :: Int -> String -> String
cjustify n s = spaces halfm ++ s ++ spaces (m - halfm)
  where
    m     = n - length s
    halfm = m `div` 2
ljustify n s = s ++ spaces (max 0 (n - length s))
rjustify n s = spaces (n - length s) ++ s

spaces   :: Int -> String
spaces n = replicate n ' '

hline :: String
hline = replicate 77 '-'
