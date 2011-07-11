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
   -- ** QuickCheck Properties
   -- *** Construction
   prop_validNormalised,
   prop_has,
   prop_unfold,
   -- *** Basic Operations
   prop_union,
   prop_intersection,
   prop_difference,
   prop_negation,
   prop_not_empty,
   -- *** Some Identities and Inequalities
   -- $ConstructionProperties
   -- $BasicOperationProperties
   -- $SomeIdentitiesAndInequalities
   prop_empty,
   prop_full,
   prop_empty_intersection,
   prop_full_union,
   prop_union_superset,
   prop_intersection_subset,
   prop_diff_intersect,
   prop_subset,
   prop_strict_subset,
   prop_union_strict_superset,
   prop_intersection_commutes,
   prop_union_commutes,
   prop_intersection_associates,
   prop_union_associates,
   prop_de_morgan_intersection,
   prop_de_morgan_union,
) where

import Data.Ranged.Boundaries
import Data.Ranged.Ranges
import Data.Monoid

import Data.List
import Test.QuickCheck

infixl 7 -/\-
infixl 6 -\/-, -!-
infixl 5 -<=-, -<-, -?-

-- | An RSet (for Ranged Set) is a list of ranges.  The ranges must be sorted
-- and not overlap.
newtype DiscreteOrdered v => RSet v = RSet {rSetRanges :: [Range v]}
   deriving (Eq, Show, Ord)

instance DiscreteOrdered a => Monoid (RSet a) where
    mappend = rSetUnion
    mempty = rSetEmpty

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


-- QuickCheck Generators

instance (Arbitrary v, DiscreteOrdered v, Show v) =>
      Arbitrary (RSet v)
   where
   arbitrary = frequency [
      (1, return rSetEmpty),
      (1, return rSetFull),
      (18, do
         ls <- arbitrary
         return $ makeRangedSet $ rangeList $ sort ls
      )]
      where
         -- Arbitrary lists of ranges don't give many interesting sets after
         -- normalisation.  So instead generate a sorted list of boundaries
         -- and pair them off.  Odd boundaries are dropped.
         rangeList (b1:b2:bs) = Range b1 b2 : rangeList bs
         rangeList _ = []

instance (CoArbitrary v, DiscreteOrdered v, Show v) =>
      CoArbitrary (RSet v)
   where
   coarbitrary (RSet ls) = variant (0 :: Int) . coarbitrary ls

-- ==================================================================
-- QuickCheck Properties
-- ==================================================================

---------------------------------------------------------------------
-- Construction properties
---------------------------------------------------------------------

-- | A normalised range list is valid for unsafeRangedSet
--
-- > prop_validNormalised ls = validRangeList $ normaliseRangeList ls
prop_validNormalised :: (DiscreteOrdered a) => [Range a] -> Bool
prop_validNormalised ls = validRangeList $ normaliseRangeList ls


-- | Iff a value is in a range list then it is in a ranged set
-- constructed from that list.
--
-- > prop_has ls v = (ls `rangeListHas` v) == makeRangedSet ls -?- v
prop_has :: (DiscreteOrdered a) => [Range a] -> a -> Bool
prop_has ls v = (ls `rangeListHas` v) == makeRangedSet ls -?- v


-- | Verifies the correct membership of a set containing all integers
-- starting with the digit \"1\" up to 19999.
--
-- > prop_unfold = (v <= 99999 && head (show v) == '1') == (initial1 -?- v)
-- >    where
-- >       initial1 = rSetUnfold (BoundaryBelow 1) addNines times10
-- >       addNines (BoundaryBelow n) = BoundaryAbove $ n * 2 - 1
-- >       times10 (BoundaryBelow n) =
-- >          if n <= 1000 then Just $ BoundaryBelow $ n * 10 else Nothing

prop_unfold :: Integer -> Bool
prop_unfold v = (v <= 99999 && head (show v) == '1') == (initial1 -?- v)
   where
      initial1 = rSetUnfold (BoundaryBelow 1) addNines times10
      addNines (BoundaryBelow n) = BoundaryAbove $ n * 2 - 1
      addNines _ = error "Can't happen"
      times10 (BoundaryBelow n) =
         if n <= 10000 then Just $ BoundaryBelow $ n * 10 else Nothing
      times10 _ = error "Can't happen"

---------------------------------------------------------------------
-- Basic operation properties
---------------------------------------------------------------------

-- | Iff a value is in either of two ranged sets then it is in the union of
-- those two sets.
--
-- > prop_union rs1 rs2 v =
-- >    (rs1 -?- v || rs2 -?- v) == ((rs1 -\/- rs2) -?- v)
prop_union :: (DiscreteOrdered a ) => RSet a -> RSet a -> a -> Bool
prop_union rs1 rs2 v = (rs1 -?- v || rs2 -?- v) == ((rs1 -\/- rs2) -?- v)

-- | Iff a value is in both of two ranged sets then it is n the intersection
-- of those two sets.
--
-- > prop_intersection rs1 rs2 v =
-- >    (rs1 -?- v && rs2 -?- v) == ((rs1 -/\- rs2) -?- v)
prop_intersection :: (DiscreteOrdered a) => RSet a -> RSet a -> a -> Bool
prop_intersection rs1 rs2 v =
   (rs1 -?- v && rs2 -?- v) == ((rs1 -/\- rs2) -?- v)

-- | Iff a value is in ranged set 1 and not in ranged set 2 then it is in the
-- difference of the two.
--
-- > prop_difference rs1 rs2 v =
-- >    (rs1 -?- v && not (rs2 -?- v)) == ((rs1 -!- rs2) -?- v)
prop_difference :: (DiscreteOrdered a) => RSet a -> RSet a -> a -> Bool
prop_difference rs1 rs2 v =
   (rs1 -?- v && not (rs2 -?- v)) == ((rs1 -!- rs2) -?- v)

-- | Iff a value is not in a ranged set then it is in its negation.
--
-- > prop_negation rs v = rs -?- v == not (rSetNegation rs -?- v)
prop_negation :: (DiscreteOrdered a) => RSet a -> a -> Bool
prop_negation rs v = rs -?- v == not (rSetNegation rs -?- v)

-- | A set that contains a value is not empty
--
-- > prop_not_empty rs v = (rs -?- v) ==> not (rSetIsEmpty rs)
prop_not_empty :: (DiscreteOrdered a) => RSet a -> a -> Property
prop_not_empty rs v = (rs -?- v) ==> not (rSetIsEmpty rs)

---------------------------------------------------------------------
-- Some identities and inequalities of sets
---------------------------------------------------------------------

-- | The empty set has no members.
--
-- > prop_empty v = not (rSetEmpty -?- v)
prop_empty :: (DiscreteOrdered a) => a -> Bool
prop_empty v = not (rSetEmpty -?- v)

-- | The full set has every member.
--
-- > prop_full v = rSetFull -?- v
prop_full :: (DiscreteOrdered a) => a -> Bool
prop_full v = rSetFull -?- v

-- | The intersection of a set with its negation is empty.
--
-- > prop_empty_intersection rs =
-- >    rSetIsEmpty (rs -/\- rSetNegation rs)
prop_empty_intersection :: (DiscreteOrdered a) => RSet a -> Bool
prop_empty_intersection rs =
   rSetIsEmpty (rs -/\- rSetNegation rs)

-- | The union of a set with its negation is full.
--
-- > prop_full_union rs v =
-- >    rSetIsFull (rs -\/- rSetNegation rs)
prop_full_union :: (DiscreteOrdered a) => RSet a -> Bool
prop_full_union rs =
   rSetIsFull (rs -\/- rSetNegation rs)

-- | The union of two sets is the non-strict superset of both.
--
-- > prop_union_superset rs1 rs2 =
-- >    rs1 -<=- u && rs2 -<=- u
-- >    where
-- >       u = rs1 -\/- rs2
prop_union_superset :: (DiscreteOrdered a) => RSet a -> RSet a -> Bool
prop_union_superset rs1 rs2 =
   rs1 -<=- u && rs2 -<=- u
   where
      u = rs1 -\/- rs2

-- | The intersection of two sets is the non-strict subset of both.
--
-- > prop_intersection_subset rs1 rs2 =
-- >    i -<=- rs1 && i -<=- rs2
-- >    where
-- >       i = rs1 -/\- rs2
prop_intersection_subset :: (DiscreteOrdered a) => RSet a -> RSet a -> Bool
prop_intersection_subset rs1 rs2 = i -<=- rs1 && i -<=- rs2
   where
      i = rs1 -/\- rs2

-- | The difference of two sets intersected with the subtractand is empty.
--
-- > prop_diff_intersect rs1 rs2 =
-- >    rSetIsEmpty ((rs1 -!- rs2) -/\- rs2)
prop_diff_intersect :: (DiscreteOrdered a) => RSet a -> RSet a -> Bool
prop_diff_intersect rs1 rs2 = rSetIsEmpty ((rs1 -!- rs2) -/\- rs2)

-- | A set is the non-strict subset of itself.
--
-- > prop_subset rs = rs -<=- rs
prop_subset :: (DiscreteOrdered a) => RSet a -> Bool
prop_subset rs = rs -<=- rs

-- | A set is not the strict subset of itself.
--
-- > prop_strict_subset rs = not (rs -<- rs)
prop_strict_subset :: (DiscreteOrdered a) => RSet a -> Bool
prop_strict_subset rs = not (rs -<- rs)

-- | If rs1 - rs2 is not empty then the union of rs1 and rs2 will be a strict
-- superset of rs2.
--
-- > prop_union_strict_superset rs1 rs2 =
-- >    (not $ rSetIsEmpty (rs1 -!- rs2))
-- >    ==> (rs2 -<- (rs1 -\/- rs2))
prop_union_strict_superset :: (DiscreteOrdered a) => RSet a -> RSet a -> Property
prop_union_strict_superset rs1 rs2 =
   (not $ rSetIsEmpty (rs1 -!- rs2)) ==> (rs2 -<- (rs1 -\/- rs2))

-- | Intersection commutes.
--
-- > prop_intersection_commutes rs1 rs2 = (rs1 -/\- rs2) == (rs2 -/\- rs1)
prop_intersection_commutes :: (DiscreteOrdered a) => RSet a -> RSet a -> Bool
prop_intersection_commutes rs1 rs2 = (rs1 -/\- rs2) == (rs2 -/\- rs1)

-- | Union commutes.
--
-- > prop_union_commutes rs1 rs2 = (rs1 -\/- rs2) == (rs2 -\/- rs1)
prop_union_commutes :: (DiscreteOrdered a) => RSet a -> RSet a -> Bool
prop_union_commutes rs1 rs2 = (rs1 -\/- rs2) == (rs2 -\/- rs1)

-- | Intersection associates.
--
-- > prop_intersection_associates rs1 rs2 rs3 =
-- >    ((rs1 -/\- rs2) -/\- rs3) == (rs1 -/\- (rs2 -/\- rs3))
prop_intersection_associates :: (DiscreteOrdered a) =>
   RSet a -> RSet a  -> RSet a -> Bool
prop_intersection_associates rs1 rs2 rs3 =
   ((rs1 -/\- rs2) -/\- rs3) == (rs1 -/\- (rs2 -/\- rs3))

-- | Union associates.
--
-- > prop_union_associates rs1 rs2 rs3 =
-- >    ((rs1 -\/- rs2) -\/- rs3) == (rs1 -\/- (rs2 -\/- rs3))
prop_union_associates :: (DiscreteOrdered a) =>
   RSet a -> RSet a  -> RSet a -> Bool
prop_union_associates rs1 rs2 rs3 =
   ((rs1 -\/- rs2) -\/- rs3) == (rs1 -\/- (rs2 -\/- rs3))

-- | De Morgan's Law for Intersection.
--
-- > prop_de_morgan_intersection rs1 rs2 =
-- >    rSetNegation (rs1 -/\- rs2) == (rSetNegation rs1 -\/- rSetNegation rs2)
prop_de_morgan_intersection :: (DiscreteOrdered a) => RSet a -> RSet a -> Bool
prop_de_morgan_intersection rs1 rs2 =
   rSetNegation (rs1 -/\- rs2) == (rSetNegation rs1 -\/- rSetNegation rs2)

-- | De Morgan's Law for Union.
--
-- > prop_de_morgan_union rs1 rs2 =
-- >    rSetNegation (rs1 -\/- rs2) == (rSetNegation rs1 -/\- rSetNegation rs2)

prop_de_morgan_union :: (DiscreteOrdered a) => RSet a -> RSet a -> Bool
prop_de_morgan_union rs1 rs2 =
   rSetNegation (rs1 -\/- rs2) == (rSetNegation rs1 -/\- rSetNegation rs2)
