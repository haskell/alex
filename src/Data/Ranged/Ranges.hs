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
   -- ** QuickCheck properties
   prop_unionRange,
   prop_unionRangeLength,
   prop_intersectionRange,
   prop_differenceRange,
   prop_intersectionOverlap,
   prop_enclosureUnion,
   prop_singletonRangeHas,
   prop_singletonRangeHasOnly,
   prop_singletonRangeConverse,
   prop_emptyNonSingleton,
   prop_fullNonSingleton,
   prop_nonSingleton,
   prop_intSingleton
) where

import Control.Monad
import Data.Ranged.Boundaries
import Data.Maybe
import Test.QuickCheck

-- | A Range has upper and lower boundaries.
data Ord v => Range v = Range {rangeLower, rangeUpper :: Boundary v}

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
emptyRange :: DiscreteOrdered v => Range v
emptyRange = Range BoundaryAboveAll BoundaryBelowAll


-- | The full range.  All values are within it.
fullRange :: DiscreteOrdered v => Range v
fullRange = Range BoundaryBelowAll BoundaryAboveAll


-- | A range containing a single value
singletonRange :: DiscreteOrdered v => v -> Range v
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


-- QuickCheck generators

instance (Arbitrary v,  DiscreteOrdered v, Show v) =>
   Arbitrary (Range v) where

   arbitrary = frequency [
      (17, do  -- Ordinary range
         b1 <- arbitrary
         b2 <- arbitrary
         if b1 < b2
            then return $ Range b1 b2
            else return $ Range b2 b1
      ),
      (1, do  -- Singleton range
         v <- arbitrary
         return $ singletonRange v
      ),
      (1, return emptyRange),
      (1, return fullRange)
      ]

instance (CoArbitrary v, DiscreteOrdered v, Show v) =>
   CoArbitrary (Range v) where

   coarbitrary (Range lower upper) =
      variant (0 :: Int) . coarbitrary lower . coarbitrary upper



-- QuickCheck Properties

-- | The union of two ranges has a value iff either range has it.
--
-- > prop_unionRange r1 r2 n =
-- >    (r1 `rangeHas` n || r2 `rangeHas` n)
-- >    == (r1 `rangeUnion` r2) `rangeListHas` n
prop_unionRange :: (DiscreteOrdered a) => Range a -> Range a -> a -> Bool
prop_unionRange r1 r2 n =
   (r1 `rangeHas` n || r2 `rangeHas` n)
   == (r1 `rangeUnion` r2) `rangeListHas` n

-- | The union of two ranges always contains one or two ranges.
--
-- > prop_unionRangeLength r1 r2 = (n == 1) || (n == 2)
-- >    where n = length $ rangeUnion r1 r2
prop_unionRangeLength :: (DiscreteOrdered a) => Range a -> Range a -> Bool
prop_unionRangeLength r1 r2 = (n == 1) || (n == 2)
   where n = length $ rangeUnion r1 r2

-- | The intersection of two ranges has a value iff both ranges have it.
--
-- > prop_intersectionRange r1 r2 n =
-- >    (r1 `rangeHas` n && r2 `rangeHas` n)
-- >    == (r1 `rangeIntersection` r2) `rangeHas` n
prop_intersectionRange :: (DiscreteOrdered a) => Range a -> Range a -> a -> Bool
prop_intersectionRange r1 r2 n =
   (r1 `rangeHas` n && r2 `rangeHas` n)
   == (r1 `rangeIntersection` r2) `rangeHas` n

-- | The difference of two ranges has a value iff the first range has it and
-- the second does not.
--
-- > prop_differenceRange r1 r2 n =
-- >    (r1 `rangeHas` n && not (r2 `rangeHas` n))
-- >    == (r1 `rangeDifference` r2) `rangeListHas` n
prop_differenceRange :: (DiscreteOrdered a) => Range a -> Range a -> a -> Bool
prop_differenceRange r1 r2 n =
   (r1 `rangeHas` n && not (r2 `rangeHas` n))
   == (r1 `rangeDifference` r2) `rangeListHas` n

-- | Iff two ranges overlap then their intersection is non-empty.
--
-- > prop_intersectionOverlap r1 r2 =
-- >     (rangeIsEmpty $ rangeIntersection r1 r2) == (rangeOverlap r1 r2)
prop_intersectionOverlap :: (DiscreteOrdered a) => Range a -> Range a -> Bool
prop_intersectionOverlap r1 r2 =
    (rangeIsEmpty $ rangeIntersection r1 r2) == not (rangeOverlap r1 r2)

-- | Range enclosure makes union an identity function.
--
-- > prop_enclosureUnion r1 r2 =
-- >    rangeEncloses r1 r2 == (rangeUnion r1 r2 == [r1])
prop_enclosureUnion :: (DiscreteOrdered a) => Range a -> Range a -> Bool
prop_enclosureUnion r1 r2 = rangeEncloses r1 r2 == (rangeUnion r1 r2 == [r1])

-- | Range Singleton has its member.
--
-- > prop_singletonRangeHas v = singletonRange v `rangeHas` v
prop_singletonRangeHas :: (DiscreteOrdered a) => a -> Bool
prop_singletonRangeHas v = singletonRange v `rangeHas` v

-- | Range Singleton has only its member.
--
-- > prop_singletonHasOnly v1 v2 =
-- >    (v1 == v2) == (singletonRange v1 `rangeHas` v2)
prop_singletonRangeHasOnly :: (DiscreteOrdered a) => a -> a -> Bool
prop_singletonRangeHasOnly v1 v2 =
   (v1 == v2) == (singletonRange v1 `rangeHas` v2)

-- | A singleton range can have its value extracted.
--
-- > prop_singletonRangeConverse v =
-- >    rangeSingletonValue (singletonRange v) == Just v
prop_singletonRangeConverse:: (DiscreteOrdered a) => a -> Bool
prop_singletonRangeConverse v =
   rangeSingletonValue (singletonRange v) == Just v

-- | The empty range is not a singleton.
--
-- > prop_emptyNonSingleton = rangeSingletonValue emptyRange == Nothing
prop_emptyNonSingleton :: Bool
prop_emptyNonSingleton =
    rangeSingletonValue (emptyRange :: Range Int) == Nothing

-- | The full range is not a singleton.
--
-- > prop_fullNonSingleton = rangeSingletonValue fullRange == Nothing
prop_fullNonSingleton :: Bool
prop_fullNonSingleton =
    rangeSingletonValue (fullRange :: Range Int) == Nothing

-- | For real x and y, @x < y@ implies that any range between them is a
-- non-singleton.
prop_nonSingleton :: Double -> Double -> Property
prop_nonSingleton x y = (x < y) ==> null $ mapMaybe rangeSingletonValue rs
   where rs = [
          Range (BoundaryBelow x) (BoundaryBelow y),
          Range (BoundaryAbove x) (BoundaryBelow y),
          Range (BoundaryBelow x) (BoundaryAbove y),
          Range (BoundaryAbove x) (BoundaryAbove y)]


-- | For all integers x and y, any range formed from boundaries on either side
-- of x and y is a singleton iff it contains exactly one integer.
prop_intSingleton :: Integer -> Integer -> Property
prop_intSingleton x y = forAll (rangeAround x y) $ \r ->
                        case filter (rangeHas r) [x-1 .. y+1] of
                          [v]  -> rangeSingletonValue r == Just v
                          _    -> rangeSingletonValue r == Nothing
    where
      rangeAround v1 v2 = return Range `ap` genBound v1 `ap` genBound v2
      genBound v = elements [BoundaryAbove v, BoundaryBelow v]



