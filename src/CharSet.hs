-- -----------------------------------------------------------------------------
-- 
-- CharSet.hs, part of Alex
--
-- (c) Chris Dornan 1995-2000, Simon Marlow 2003
--
-- An abstract CharSet type for Alex.  To begin with we'll use Alex's
-- original definition of sets as functions, then later will
-- transition to something that will work better with Unicode.
--
-- ----------------------------------------------------------------------------}

module CharSet (
  CharSet, -- abstract
  emptyCharSet,
  charSetSingleton,
  charSet,
  charSetMinus,
  charSetComplement,
  charSetRange,
  charSetUnion,
  charSetToArray
  ) where

import Data.Array

-- Implementation as functions
type CharSet = Char -> Bool

emptyCharSet = const False

charSetSingleton :: Char -> CharSet
charSetSingleton c = \x -> x == c

charSet :: [Char] -> CharSet
charSet s x = x `elem` s

charSetMinus :: CharSet -> CharSet -> CharSet
charSetMinus s1 s2 x = s1 x && not (s2 x)

charSetUnion :: CharSet -> CharSet -> CharSet
charSetUnion s1 s2 x = s1 x || s2 x

charSetComplement :: CharSet -> CharSet
charSetComplement s1 = not . s1

charSetRange :: Char -> Char -> CharSet
charSetRange c1 c2 x = x >= c1 && x <= c2

charSetToArray :: CharSet -> Array Char Bool
charSetToArray set = array (fst (head ass), fst (last ass)) ass
  where ass = [(c,set c) | c <- ['\0'..'\xff']]
