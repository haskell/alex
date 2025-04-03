{-# LANGUAGE CPP #-}

module DFAMin (minimizeDFA) where

import AbsSyn

import Control.Monad (guard)
import Data.Foldable (fold)
import Data.IntMap   (IntMap)
import Data.IntSet   (IntSet)
import Data.Map      (Map)

import qualified Data.IntMap   as IntMap
import qualified Data.IntSet   as IntSet
import qualified Data.List     as List
import qualified Data.Map      as Map

{- Note [Hopcroft's Algorithm]

DFA minimization is implemented using Hopcroft's algorithm. It is defined on
Wikipedia as follows.

We assume the following definitions:
    - Q is the set of all states in our DFA
    - F is the subset of Q that contains all final (or "accepting") states
    - ∑ is the set of input symbols (for us, [0..255])

We use the phrase  "X refines Y into Y1 and Y2" to mean the following:
    - Y1 := Y ∩ X
    - Y2 := Y \ X
    - |Y1| > 0
    - |Y2| > 0

The algorithm itself is defined thusly:

    P := {F, Q \ F}
    W := {F, Q \ F}
    while (W is not empty) do
      choose and remove a set A from W
      for each c in Σ do
        let X be the set of states for which a transition on c leads to a state in A
        for each set Y in P that is refined by X into Y1 and Y2 do
            replace Y in P by the two sets Y1 and Y2
            if Y is in W
                replace Y in W by Y1 and Y2
            else
                if |Y1| <= |Y2|
                    add Y1 to W
                else
                    add Y2 to W


Our implementation differs slightly, as we perform several optimizations.

In the Wikipedia implementation, P and W are initialized to subsets of all sets
of Q, specifically F and Q \ F. The exact subsets do not matter; what matters is
the following:
    - all states in Q should be in W
    - equivalent states should all be in the same subset

As per the first requirement, it would be fine for P and W to be initialized
with a set that only contains Q. The second requirement stems from the fact that
our partition "refining" can divide subsets, but we do not have a way to
re-merge subsets.

Our first optimization is that we use a more granular division of states in the
initial set. Specifically, we group all states by their list of "accepts", since
we know that for two states to be equivalent their list of "accepts" must be the
same: the resulting subsets therefore meet our two stated criteria.


Our second optimization relies on the observation that given that all states are
in W, then all states will appear in A; as a result, instead of starting with a
set P that contains all subsets, that we refine in parallel to W, we can instead
start with an empty set R, and add each A to R before iterating over P. This
makes updating R and W easier, and removes the need for the expensive "is Y in
W" check.


With those two optimizations, our implementation is therefore:

    R := {}
    W := {all "accept" subsets of Q}
    while (W is not empty) do
      choose and remove a set A from W
      add A to R
      for each c in Σ do
        let X be the set of states for which a transition on c leads to a state in A
        for each set Y in R that is refined by X into Y1 and Y2 do
            replace Y in R by the two sets Y1 and Y2
            if |Y1| <= |Y2|
                add Y1 to Q
            else
                add Y2 to Q
        for each set Y in Q that is refined by X into Y1 and Y2 do
            replace Y in Q by the two sets Y1 and Y2


-}

-- | Reduce the number of states in the given DFA by grouping indistinguishable
-- states.
minimizeDFA :: forall a. Ord a => DFA Int a -> DFA Int a
minimizeDFA dfa@(DFA starts statemap) = DFA starts (Map.fromList states)
  where
    equiv_classes :: [EquivalenceClass]
    equiv_classes = groupEquivalentStates dfa

    numbered_states :: [(Int, EquivalenceClass)]
    numbered_states = number (length starts) equiv_classes

    -- assign each state in the minimized DFA a number, making
    -- sure that we assign the numbers [0..] to the start states.
    number :: Int -> [EquivalenceClass] -> [(Int, EquivalenceClass)]
    number _ [] = []
    number n (ss:sss) =
      case filter (`IntSet.member` ss) starts of
        []      -> (n,ss) : number (n+1) sss
        starts' -> map (,ss) starts' ++ number n sss
        -- if one of the states of the minimized DFA corresponds
        -- to multiple starts states, we just have to duplicate
        -- that state.

    states :: [(Int, State Int a)]
    states = do
      (n, equiv) <- numbered_states
      let old_states = map (lookupOrPanic statemap) (IntSet.toList equiv)
          accepts = map fix_acc $ state_acc $ headOrPanic old_states
          transitions = IntMap.fromList $ do
            State _ out <- old_states
            (b, old) <- IntMap.toList out
            pure (b, get_new old)
      pure (n, State accepts transitions)

    fix_acc :: Accept a -> Accept a
    fix_acc acc = acc { accRightCtx = fix_rctxt (accRightCtx acc) }

    fix_rctxt :: RightContext SNum -> RightContext SNum
    fix_rctxt (RightContextRExp s) = RightContextRExp (get_new s)
    fix_rctxt other                = other

    get_new :: Int -> Int
    get_new = lookupOrPanic old_to_new

    old_to_new :: Map Int Int
    old_to_new = Map.fromList $ do
      (n,ss) <- numbered_states
      s <- IntSet.toList ss
      pure (s,n)

    headOrPanic :: forall x. [x] -> x
    headOrPanic []    = error "minimizeDFA: empty equivalence class"
    headOrPanic (x:_) = x

    lookupOrPanic :: forall x. Map Int x -> Int -> x
    lookupOrPanic m k = case Map.lookup k m of
      Nothing -> error "minimizeDFA: state not found"
      Just x  -> x


type EquivalenceClass = IntSet


-- | Creates the subset of Q that are used to initialize W.
--
-- As per the two conditions listed in Note [Hopcroft's Algorithm], we have two
-- requirements: the union of all resulting sets must be equivalent to Q the set
-- of all states, and all equivalent states must be in the same subsets.
--
-- We group states by their list of 'Accept'.
initialSubsets :: forall a. Ord a => DFA Int a -> [EquivalenceClass]
initialSubsets dfa = Map.elems $ Map.fromListWith IntSet.union $ do
  (stateIndex, State accepts _transitions) <- Map.toList $ dfa_states dfa
  pure (accepts, IntSet.singleton stateIndex)


-- | Creates a cache of all reverse transitions for a given DFA.
--
-- To each token c in Σ, the resulting map contains a reverse map of
-- transitions. That is, for each c, we have a map that, to a state
-- s, associate the set of states that can reach s via c.
--
-- Given that the actual value of c is never actually required, we flatten the
-- result into a list.
generateReverseTransitionCache :: forall a. Ord a => DFA Int a -> [IntMap EquivalenceClass]
generateReverseTransitionCache dfa = IntMap.elems $
  IntMap.fromListWith (IntMap.unionWith IntSet.union) $ do
    (startingState, stateInfo) <- Map.toList $ dfa_states dfa
    (token, targetState) <- IntMap.toList $ state_out stateInfo
    pure (token, IntMap.singleton targetState (IntSet.singleton startingState))


-- | Given an IntMap and an IntSet, restrict the IntMap to the keys that are
-- within the IntSet.
--
-- This function is a simple wrapper around 'IntMap.restrictKeys',
-- provided for compatibility with older versions of containers.
restrictKeys :: forall a. IntMap a -> IntSet -> IntMap a
restrictKeys m s =
#if MIN_VERSION_containers(0,6,0)
    IntMap.restrictKeys m s
#else
    IntMap.filterWithKey (\k _ -> k `IntSet.member` s) m
#endif


-- | Given two sets X and Y, compute their intersection and difference.
-- Only returns both if both are non-empty, otherwise return neither.
refine
  :: EquivalenceClass
  -> EquivalenceClass
  -> Maybe (EquivalenceClass, EquivalenceClass)
refine x y =
  if IntSet.null intersection || IntSet.null difference
    then Nothing
    else Just (intersection, difference)
  where
    intersection = IntSet.intersection y x
    difference   = IntSet.difference   y x


-- | Given a DFA, compute all sets of equivalent states.
--
-- See Note [Hopcroft's Algorithm] for details.
groupEquivalentStates :: forall a. Ord a => DFA Int a -> [EquivalenceClass]
groupEquivalentStates dfa = outerLoop ([], initialSubsets dfa)
  where
    reverseTransitionCache :: [IntMap EquivalenceClass]
    reverseTransitionCache = generateReverseTransitionCache dfa

    -- While W isn't empty, pick an A from W, add it to R
    -- and iterate on X for each c in ∑.
    outerLoop :: ([EquivalenceClass], [EquivalenceClass]) -> [EquivalenceClass]
    outerLoop (r,  []) = r
    outerLoop (r, a:w) = outerLoop $ List.foldl' refineWithX (a:r,w) $ do
      allPreviousStates <- reverseTransitionCache
      let x = fold $ restrictKeys allPreviousStates a
      guard $ not $ IntSet.null x
      pure x

    -- Given X, refine values in R, refine values in W, and finally combine the
    -- results to obtain the new values of R an W.
    -- We can do both steps in parallel, since the new values to add in W while
    -- we process R are already defined and don't need to be processed when
    -- iterating over the original value of W.
    refineWithX (r, w) x =
      let (r', w') = unzip $ map (processR x) r
          w''      = concatMap (processW x) w
      in (concat r', concat w' ++ w'')

    processR x y = case refine x y of
      Nothing -> ([y], [])
      Just (y1, y2)
        | IntSet.size y1 <= IntSet.size y2 -> ([y2], [y1])
        | otherwise                        -> ([y1], [y2])

    processW x y = case refine x y of
      Nothing       -> [y]
      Just (y1, y2) -> [y1, y2]
