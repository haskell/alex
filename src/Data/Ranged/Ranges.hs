-----------------------------------------------------------------------------
--
-- Module      :  Data.Ranged.Ranges
-- Copyright   :  (c) Paul Johnson 2006
-- License     :  BSD-style
-- Maintainer  :  paul@cogito.org.uk
-- Stability   :  experimental
-- Portability :  portable
--
-----------------------------------------------------------------------------

-- | A range has an upper and lower boundary.
module Data.Ranged.Ranges (
   -- ** Construction
   Range (..),
   emptyRange,
   fullRange,
   -- ** Predicates
   rangeIsEmpty,
   rangeIsFull,
   rangeOverlap,
   rangeEncloses,
   rangeSingletonValue,
   -- ** Membership
   rangeHas,
   rangeListHas,
   -- ** Set Operations
   singletonRange,
   rangeIntersection,
   rangeUnion,
   rangeDifference,
) where

import           Data.Ranged.Boundaries

-- | A Range has upper and lower boundaries.
data Range v = Range {rangeLower, rangeUpper :: Boundary v}

instance (DiscreteOrdered a) => Eq (Range a) where
   r1 == r2   = (rangeIsEmpty r1 && rangeIsEmpty r2) ||
                (rangeLower r1 == rangeLower r2 &&
                 rangeUpper r1 == rangeUpper r2)


instance (DiscreteOrdered a) => Ord (Range a) where
   compare r1 r2
      | r1 == r2       = EQ
      | rangeIsEmpty r1  = LT
      | rangeIsEmpty r2  = GT
      | otherwise      = compare (rangeLower r1, rangeUpper r1)
                                 (rangeLower r2, rangeUpper r2)

instance (Show a, DiscreteOrdered a) => Show (Range a) where
   show r
      | rangeIsEmpty r     = "Empty"
      | rangeIsFull r      = "All x"
      | otherwise          =
         case rangeSingletonValue r of
            Just v  -> "x == " ++ show v
            Nothing -> lowerBound ++ "x" ++ upperBound
      where
         lowerBound = case rangeLower r of
            BoundaryBelowAll -> ""
            BoundaryBelow v  -> show v ++ " <= "
            BoundaryAbove v  -> show v ++ " < "
            BoundaryAboveAll -> error "show Range: lower bound is BoundaryAboveAll"
         upperBound = case rangeUpper r of
            BoundaryBelowAll -> error "show Range: upper bound is BoundaryBelowAll"
            BoundaryBelow v  -> " < " ++ show v
            BoundaryAbove v  -> " <= " ++ show v
            BoundaryAboveAll -> ""


-- | True if the value is within the range.
rangeHas :: Ord v => Range v -> v -> Bool

rangeHas (Range b1 b2) v =
   (v />/ b1) && not (v />/ b2)


-- | True if the value is within one of the ranges.
rangeListHas :: Ord v =>
   [Range v] -> v -> Bool
rangeListHas ls v = or $ map (\r -> rangeHas r v) ls


-- | The empty range
emptyRange :: Range v
emptyRange = Range BoundaryAboveAll BoundaryBelowAll


-- | The full range.  All values are within it.
fullRange :: Range v
fullRange = Range BoundaryBelowAll BoundaryAboveAll


-- | A range containing a single value
singletonRange :: v -> Range v
singletonRange v = Range (BoundaryBelow v) (BoundaryAbove v)


-- | If the range is a singleton, returns @Just@ the value.  Otherwise returns
-- @Nothing@.
--
-- Known bug: This always returns @Nothing@ for ranges including
-- @BoundaryBelowAll@ or @BoundaryAboveAll@.  For bounded types this can be
-- incorrect.  For instance, the following range only contains one value:
--
-- >    Range (BoundaryBelow maxBound) BoundaryAboveAll
rangeSingletonValue :: DiscreteOrdered v => Range v -> Maybe v
rangeSingletonValue (Range (BoundaryBelow v1) (BoundaryBelow v2))
   | adjacent v1 v2  = Just v1
   | otherwise       = Nothing
rangeSingletonValue (Range (BoundaryBelow v1) (BoundaryAbove v2))
   | v1 == v2        = Just v1
   | otherwise       = Nothing
rangeSingletonValue (Range (BoundaryAbove v1) (BoundaryBelow v2)) =
   do
      v2' <- adjacentBelow v2
      v2'' <- adjacentBelow v2'
      if v1 == v2'' then return v2' else Nothing
rangeSingletonValue (Range (BoundaryAbove v1) (BoundaryAbove v2))
   | adjacent v1 v2  = Just v2
   | otherwise       = Nothing
rangeSingletonValue (Range _ _) = Nothing

-- | A range is empty unless its upper boundary is greater than its lower
-- boundary.
rangeIsEmpty :: DiscreteOrdered v => Range v -> Bool
rangeIsEmpty (Range lower upper) = upper <= lower


-- | A range is full if it contains every possible value.
rangeIsFull :: DiscreteOrdered v => Range v -> Bool
rangeIsFull = (== fullRange)

-- | Two ranges overlap if their intersection is non-empty.
rangeOverlap :: DiscreteOrdered v => Range v -> Range v -> Bool
rangeOverlap r1 r2 =
   not (rangeIsEmpty r1)
   && not (rangeIsEmpty r2)
   && not (rangeUpper r1 <= rangeLower r2 || rangeUpper r2 <= rangeLower r1)


-- | The first range encloses the second if every value in the second range is
-- also within the first range.  If the second range is empty then this is
-- always true.
rangeEncloses :: DiscreteOrdered v => Range v -> Range v -> Bool
rangeEncloses r1 r2 =
   (rangeLower r1 <= rangeLower r2 && rangeUpper r2 <= rangeUpper r1)
   || rangeIsEmpty r2


-- | Intersection of two ranges, if any.
rangeIntersection :: DiscreteOrdered v => Range v -> Range v -> Range v
rangeIntersection r1@(Range lower1 upper1) r2@(Range lower2 upper2)
    | rangeIsEmpty r1 || rangeIsEmpty r2  = emptyRange
    | otherwise  = Range (max lower1 lower2) (min upper1 upper2)


-- | Union of two ranges.  Returns one or two results.
--
-- If there are two results then they are guaranteed to have a non-empty
-- gap in between, but may not be in ascending order.
rangeUnion :: DiscreteOrdered v => Range v -> Range v -> [Range v]
rangeUnion r1@(Range lower1 upper1) r2@(Range lower2 upper2)
   | rangeIsEmpty r1  = [r2]
   | rangeIsEmpty r2  = [r1]
   | otherwise =
       if touching then [Range lower upper] else [r1, r2]
   where
     touching = (max lower1 lower2) <= (min upper1 upper2)
     lower = min lower1 lower2
     upper = max upper1 upper2


-- | @range1@ minus @range2@.  Returns zero, one or two results.  Multiple
-- results are guaranteed to have non-empty gaps in between, but may not be in
-- ascending order.
rangeDifference :: DiscreteOrdered v => Range v -> Range v -> [Range v]

rangeDifference r1@(Range lower1 upper1) (Range lower2 upper2) =
   -- There are six possibilities
   --    1: r2 completely less than r1
   --    2: r2 overlaps bottom of r1
   --    3: r2 encloses r1
   --    4: r1 encloses r2
   --    5: r2 overlaps top of r1
   --    6: r2 completely greater than r1
   if intersects
      then -- Cases 2,3,4,5
         filter (not . rangeIsEmpty) [Range lower1 lower2, Range upper2 upper1]
      else -- Cases 1, 6
         [r1]
   where
      intersects = (max lower1 lower2) < (min upper1 upper2)
