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

{-# LANGUAGE OverloadedLists #-}

module CharSet (
  setSingleton,

  Encoding(..),

  Byte,
  ByteSet,
  byteSetSingleton,
  byteRanges,
  byteSetRange,

  CharSet, -- abstract
  emptyCharSet,
  charSetSingleton,
  charSet,
  charSetMinus,
  charSetComplement,
  charSetRange,
  charSetUnion,
  charSetQuote,
  setUnions,
  byteSetToArray,
  byteSetElems,
  byteSetElem
  ) where

import           Data.Array         ( Array, array )
import           Data.Char          ( chr, ord )
import           Data.Maybe         ( catMaybes )
import           Data.Word          ( Word8 )
import           Data.List.NonEmpty ( pattern (:|), (<|) )
import qualified Data.List.NonEmpty as List1

import           UTF8               ( List1, encode )
import           Data.Ranged
  ( Boundary( BoundaryAbove, BoundaryAboveAll, BoundaryBelow, BoundaryBelowAll )
  , DiscreteOrdered, Range( Range ), RSet
  , makeRangedSet
  , rSetDifference, rSetEmpty, rSetHas, rSetNegation, rSetRanges, rSetUnion, rSingleton
  )

-- import Data.Semigroup (sconcat)
-- import qualified Data.Foldable      as Fold

type Byte = Word8
-- Implementation as functions
type CharSet = RSet Char
type ByteSet = RSet Byte
-- type Utf8Set = RSet [Byte]
type Utf8Range = Span (List1 Byte)

data Encoding = Latin1 | UTF8
              deriving (Eq, Show)

emptyCharSet :: CharSet
emptyCharSet = rSetEmpty

byteSetElem :: ByteSet -> Byte -> Bool
byteSetElem = rSetHas

charSetSingleton :: Char -> CharSet
charSetSingleton = rSingleton

setSingleton :: DiscreteOrdered a => a -> RSet a
setSingleton = rSingleton

charSet :: [Char] -> CharSet
charSet = setUnions . fmap charSetSingleton

charSetMinus :: CharSet -> CharSet -> CharSet
charSetMinus = rSetDifference

charSetUnion :: CharSet -> CharSet -> CharSet
charSetUnion = rSetUnion

setUnions :: DiscreteOrdered a => [RSet a] -> RSet a
setUnions = foldr rSetUnion rSetEmpty

charSetComplement :: CharSet -> CharSet
charSetComplement = rSetNegation

charSetRange :: Char -> Char -> CharSet
charSetRange c1 c2 = makeRangedSet [Range (BoundaryBelow c1) (BoundaryAbove c2)]

{-# INLINE bytes #-}
bytes :: [Byte]
bytes = [minBound..maxBound]

byteSetToArray :: ByteSet -> Array Byte Bool
byteSetToArray set = array (minBound, maxBound) [(c, rSetHas set c) | c <- bytes]

byteSetElems :: ByteSet -> [Byte]
byteSetElems set = filter (rSetHas set) bytes

charToRanges :: Encoding -> CharSet -> [Utf8Range]
charToRanges Latin1 =
    map (fmap ((:| []) . fromIntegral . ord)) -- Span [Byte]
  . catMaybes
  . fmap (charRangeToCharSpan False)
  . rSetRanges
charToRanges UTF8 =
    concat                  -- Span [Byte]
  . fmap toUtfRange         -- [Span [Byte]]
  . fmap (fmap UTF8.encode) -- Span [Byte]
  . catMaybes
  . fmap (charRangeToCharSpan True)
  . rSetRanges

-- | Turns a range of characters expressed as a pair of UTF-8 byte sequences into a set of ranges, in which each range of the resulting set is between pairs of sequences of the same length
toUtfRange :: Span (List1 Byte) -> [Span (List1 Byte)]
toUtfRange (Span x y) = List1.toList $ fix x y

fix :: List1 Byte -> List1 Byte -> List1 (Span (List1 Byte))
fix x y
    | length x == length y = [Span x y]
    | length x == 1 = Span x [0x7F] <| fix [0xC2,0x80] y
    | length x == 2 = Span x [0xDF,0xBF] <| fix [0xE0,0x80,0x80] y
    | length x == 3 = Span x [0xEF,0xBF,0xBF] <| fix [0xF0,0x80,0x80,0x80] y
    | otherwise = error "fix: incorrect input given"


byteRangeToBytePair :: Span a -> (a, a)
byteRangeToBytePair (Span x y) = (x, y)

data Span a = Span a a -- lower bound inclusive, higher bound exclusive
                       -- (SDM: upper bound inclusive, surely?)
instance Functor Span where
    fmap f (Span x y) = Span (f x) (f y)

charRangeToCharSpan :: Bool -> Range Char -> Maybe (Span Char)
charRangeToCharSpan _ (Range BoundaryAboveAll _) = Nothing
charRangeToCharSpan _ (Range (BoundaryAbove c) _) | c == maxBound = Nothing
charRangeToCharSpan _ (Range _ BoundaryBelowAll) = Nothing
charRangeToCharSpan _ (Range _ (BoundaryBelow c)) | c == minBound = Nothing
charRangeToCharSpan uni (Range x y) = Just (Span (l x) (h y))
    where l b = case b of
            BoundaryBelowAll -> '\0'
            BoundaryBelow a  -> a
            BoundaryAbove a  -> succ a
            BoundaryAboveAll -> error "panic: charRangeToCharSpan"
          h b = case b of
            BoundaryBelowAll -> error "panic: charRangeToCharSpan"
            BoundaryBelow a  -> pred a
            BoundaryAbove a  -> a
            BoundaryAboveAll | uni -> chr 0x10ffff
                             | otherwise -> chr 0xff

byteRanges :: Encoding -> CharSet -> [(List1 Byte, List1 Byte)]
byteRanges enc = fmap byteRangeToBytePair . charToRanges enc

byteSetRange :: Byte -> Byte -> ByteSet
byteSetRange c1 c2 = makeRangedSet [Range (BoundaryBelow c1) (BoundaryAbove c2)]

byteSetSingleton :: Byte -> ByteSet
byteSetSingleton = rSingleton

-- TODO: More efficient generated code!
charSetQuote :: CharSet -> String
charSetQuote s = "(\\c -> " ++ foldr (\x y -> x ++ " || " ++ y) "False" (map quoteRange (rSetRanges s)) ++ ")"
    where quoteRange (Range l h) = quoteL l ++ " && " ++ quoteH h
          quoteL (BoundaryAbove a) = "c > " ++ show a
          quoteL (BoundaryBelow a) = "c >= " ++ show a
          quoteL (BoundaryAboveAll) = "False"
          quoteL (BoundaryBelowAll) = "True"
          quoteH (BoundaryAbove a) = "c <= " ++ show a
          quoteH (BoundaryBelow a) = "c < " ++ show a
          quoteH (BoundaryAboveAll) = "True"
          quoteH (BoundaryBelowAll) = "False"
