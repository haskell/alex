-- -----------------------------------------------------------------------------
-- 
-- Util.hs, part of Alex
--
-- (c) Simon Marlow 2003
--
-- General utilities used in various parts of Alex
--
-- ----------------------------------------------------------------------------}

module Util where

-- Pretty-printing utilities

str = showString
char c = (c :)

nl = char '\n'

paren s = char '(' . s . char ')'
brack s = char '[' . s . char ']'

interleave_shows s [] = id
interleave_shows s xs = foldr1 (\a b -> a . s . b) xs

space = char ' '

cjustify, ljustify, rjustify :: Int -> String -> String
cjustify n s = spaces halfm ++ s ++ spaces (m - halfm)
               where m     = n - length s
                     halfm = m `div` 2
ljustify n s = s ++ spaces (max 0 (n - length s))
rjustify n s = spaces (n - length s) ++ s

spaces   :: Int -> String
spaces n = replicate n ' '

hline = replicate 77 '-'
