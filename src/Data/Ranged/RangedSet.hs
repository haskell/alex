module Data.Ranged.RangedSet (
   -- ** Ranged Set Type
   RSet,
   rSetRanges,
   -- ** Ranged Set construction functions and their preconditions
   makeRangedSet,
   unsafeRangedSet,
   validRangeList,
   normaliseRangeList,
   rSingleton,
   rSetUnfold,
   -- ** Predicates
   rSetIsEmpty,
   rSetIsFull,
   (-?-),  rSetHas,
   (-<=-), rSetIsSubset,
   (-<-),  rSetIsSubsetStrict,
   -- ** Set Operations
   (-\/-), rSetUnion,
   (-/\-), rSetIntersection,
   (-!-),  rSetDifference,
   rSetNegation,
   -- ** Useful Sets
   rSetEmpty,
   rSetFull,
) where

import Data.Ranged.Boundaries
import Data.Ranged.Ranges
#if __GLASGOW_HASKELL__ >= 800
import Data.Semigroup
#elif __GLASGOW_HASKELL__ < 710
import Data.Monoid
#endif

import Data.List

infixl 7 -/\-
infixl 6 -\/-, -!-
infixl 5 -<=-, -<-, -?-

-- | An RSet (for Ranged Set) is a list of ranges.  The ranges must be sorted
-- and not overlap.
newtype DiscreteOrdered v => RSet v = RSet {rSetRanges :: [Range v]}
   deriving (Eq, Show, Ord)

#if __GLASGOW_HASKELL__ >= 800
instance DiscreteOrdered a => Semigroup (RSet a) where
    (<>) = rSetUnion

instance DiscreteOrdered a => Monoid (RSet a) where
    mappend = (<>)
    mempty = rSetEmpty
#else
instance DiscreteOrdered a => Monoid (RSet a) where
    mappend = rSetUnion
    mempty = rSetEmpty
#endif

-- | Determine if the ranges in the list are both in order and non-overlapping.
-- If so then they are suitable input for the unsafeRangedSet function.
validRangeList :: DiscreteOrdered v => [Range v] -> Bool

validRangeList [] = True
validRangeList [Range lower upper] = lower <= upper
validRangeList rs = and $ zipWith okAdjacent rs (tail rs)
   where
      okAdjacent (Range lower1 upper1) (Range lower2 upper2) =
         lower1 <= upper1 && upper1 <= lower2 && lower2 <= upper2


-- | Rearrange and merge the ranges in the list so that they are in order and
-- non-overlapping.
normaliseRangeList :: DiscreteOrdered v => [Range v] -> [Range v]
normaliseRangeList = normalise . sort . filter (not . rangeIsEmpty)


-- Private routine: normalise a range list that is known to be already sorted.
-- This precondition is not checked.
normalise :: DiscreteOrdered v => [Range v] -> [Range v]
normalise (r1:r2:rs) =
         if overlap r1 r2
               then normalise $
                       Range (rangeLower r1)
                             (max (rangeUpper r1) (rangeUpper r2))
                       : rs
               else r1 : (normalise $ r2 : rs)
   where
      overlap (Range _ upper1) (Range lower2 _) = upper1 >= lower2

normalise rs = rs


-- | Create a new Ranged Set from a list of ranges.  The list may contain
-- ranges that overlap or are not in ascending order.
makeRangedSet :: DiscreteOrdered v => [Range v] -> RSet v
makeRangedSet = RSet . normaliseRangeList


-- | Create a new Ranged Set from a list of ranges. @validRangeList ranges@
-- must return @True@.  This precondition is not checked.
unsafeRangedSet :: DiscreteOrdered v => [Range v] -> RSet v
unsafeRangedSet = RSet

-- | Create a Ranged Set from a single element.
rSingleton :: DiscreteOrdered v => v -> RSet v
rSingleton v = unsafeRangedSet [singletonRange v]

-- | True if the set has no members.
rSetIsEmpty :: DiscreteOrdered v => RSet v -> Bool
rSetIsEmpty = null . rSetRanges


-- | True if the negation of the set has no members.
rSetIsFull :: DiscreteOrdered v => RSet v -> Bool
rSetIsFull = rSetIsEmpty . rSetNegation


-- | True if the value is within the ranged set.  Infix precedence is left 5.
rSetHas, (-?-) :: DiscreteOrdered v => RSet v -> v -> Bool
rSetHas (RSet ls) value = rSetHas1 ls
   where
      rSetHas1 [] = False
      rSetHas1 (r:rs)
         | value />/ rangeLower r = rangeHas r value || rSetHas1 rs
         | otherwise              = False

(-?-) = rSetHas

-- | True if the first argument is a subset of the second argument, or is
-- equal.
--
-- Infix precedence is left 5.
rSetIsSubset, (-<=-) :: DiscreteOrdered v => RSet v -> RSet v -> Bool
rSetIsSubset rs1 rs2 = rSetIsEmpty (rs1 -!- rs2)
(-<=-) = rSetIsSubset


-- | True if the first argument is a strict subset of the second argument.
--
-- Infix precedence is left 5.
rSetIsSubsetStrict, (-<-) :: DiscreteOrdered v => RSet v -> RSet v -> Bool
rSetIsSubsetStrict rs1 rs2 =
   rSetIsEmpty (rs1 -!- rs2)
   && not (rSetIsEmpty (rs2 -!- rs1))

(-<-) = rSetIsSubsetStrict

-- | Set union for ranged sets.  Infix precedence is left 6.
rSetUnion, (-\/-) :: DiscreteOrdered v => RSet v -> RSet v -> RSet v
-- Implementation note: rSetUnion merges the two lists into a single
-- sorted list and then calls normalise to combine overlapping ranges.
rSetUnion (RSet ls1) (RSet ls2) = RSet $ normalise $ merge ls1 ls2
   where
      merge ms1 [] = ms1
      merge [] ms2 = ms2
      merge ms1@(h1:t1) ms2@(h2:t2) =
         if h1 <  h2
            then h1 : merge t1 ms2
            else h2 : merge ms1 t2

(-\/-) = rSetUnion

-- | Set intersection for ranged sets.  Infix precedence is left 7.
rSetIntersection, (-/\-) :: DiscreteOrdered v => RSet v -> RSet v -> RSet v
rSetIntersection (RSet ls1) (RSet ls2) =
   RSet $ filter (not . rangeIsEmpty) $ merge ls1 ls2
   where
      merge ms1@(h1:t1) ms2@(h2:t2) =
         rangeIntersection h1 h2
         : if rangeUpper h1 < rangeUpper h2
               then merge t1 ms2
               else merge ms1 t2
      merge _ _ = []

(-/\-) = rSetIntersection


-- | Set difference.  Infix precedence is left 6.
rSetDifference, (-!-) :: DiscreteOrdered v => RSet v -> RSet v -> RSet v
rSetDifference rs1 rs2 = rs1 -/\- (rSetNegation rs2)
(-!-) = rSetDifference


-- | Set negation.
rSetNegation :: DiscreteOrdered a => RSet a -> RSet a
rSetNegation set = RSet $ ranges1 $ setBounds1
   where
      ranges1 (b1:b2:bs) = Range b1 b2 : ranges1 bs
      ranges1 [BoundaryAboveAll] = []
      ranges1 [b] = [Range b BoundaryAboveAll]
      ranges1 _ = []
      setBounds1 = case setBounds of
         (BoundaryBelowAll : bs)  -> bs
         _                        -> BoundaryBelowAll : setBounds
      setBounds = bounds $ rSetRanges set
      bounds (r:rs) = rangeLower r : rangeUpper r : bounds rs
      bounds _ = []

-- | The empty set.
rSetEmpty :: DiscreteOrdered a => RSet a
rSetEmpty = RSet []

-- | The set that contains everything.
rSetFull :: DiscreteOrdered a => RSet a
rSetFull = RSet [Range BoundaryBelowAll BoundaryAboveAll]

-- | Construct a range set.
rSetUnfold :: DiscreteOrdered a =>
   Boundary a
      -- ^ A first lower boundary.
   -> (Boundary a -> Boundary a)
      -- ^ A function from a lower boundary to an upper boundary, which must
      -- return a result greater than the argument (not checked).
   -> (Boundary a -> Maybe (Boundary a))
      -- ^ A function from a lower boundary to @Maybe@ the successor lower
      -- boundary, which must return a result greater than the argument
      -- (not checked).  If ranges overlap then they will be merged.
   -> RSet a
rSetUnfold bound upperFunc succFunc = RSet $ normalise $ ranges1 bound
   where
      ranges1 b =
         Range b (upperFunc b)
         : case succFunc b of
            Just b2 -> ranges1 b2
            Nothing -> []
