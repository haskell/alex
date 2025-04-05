module DFAMin (minimizeDFA) where

import AbsSyn

import Control.Monad  (guard)
import Data.Bifunctor (second)
import Data.IntMap    (IntMap)
import Data.IntSet    (IntSet)

import qualified Data.IntMap as IntMap
import qualified Data.IntSet as IntSet
import qualified Data.List   as List
import qualified Data.Map    as Map

{- Note [Hopcroft's Algorithm]

DFA minimization is implemented using Hopcroft's algorithm. The following
definition is mostly copied from Wikipedia.

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

In the Wikipedia implementation, P and W are initialized to two subsets of Q,
specifically F and Q \ F. The exact subsets do not matter; what matters is the
following:
    - P and W should contain all Q states
    - equivalent states should all be in the same subset

As per the first requirement, it would be fine for P and W to be initialized as
a set that only contains Q. Using more fine-grained subsets reduces the amount
of work that needs to be done. The second requirement stems from the fact that
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
                add Y1 to W
            else
                add Y2 to W
        for each set Y in W that is refined by X into Y1 and Y2 do
            replace Y in W by the two sets Y1 and Y2

-}

type OldSNum = Int -- ^ Old state number
type NewSNum = Int -- ^ New state number

-- | Reduce the number of states in the given DFA by grouping indistinguishable
-- states.
minimizeDFA :: forall a. Ord a => DFA OldSNum a -> DFA NewSNum a
minimizeDFA dfa@(DFA starts statemap) = DFA starts $ Map.fromList new_states
  where
    -- Group the states into classes according to the language they accept.
    equiv_classes :: [EquivalenceClass]
    equiv_classes = groupEquivalentStates dfa

    -- A map from new state numbers to a class of equivalent old states.
    numbered_states :: [(NewSNum, EquivalenceClass)]
    numbered_states = number (length starts) starts equiv_classes

    -- Assign each state in the minimized DFA a number, making
    -- sure that we assign the numbers [0..] to the start states.
    number :: NewSNum -> [NewSNum] -> [EquivalenceClass] -> [(NewSNum, EquivalenceClass)]
    number _ _ [] = []
    number n unassigned_starts (ss:sss)
      | null starts_ss = (n,ss) : continue (n+1)
      | otherwise      = map (,ss) starts_ss ++ continue n
        -- if one of the states of the minimized DFA corresponds
        -- to multiple starts states, we just have to duplicate
        -- that state.
      where
        -- All the start states in ss (starts_ss) are assigned this equivalence class.
        -- The remaining ones are passed to the recursive call.
        (starts_ss, starts_other) = List.partition (`IntSet.member` ss) unassigned_starts
        continue n' = number n' starts_other sss

    -- Mapping new state numbers to their state description.
    new_states :: [(NewSNum, State NewSNum a)]
    new_states = map (second class_to_new_state) numbered_states

    -- Translate an equivalence class of old states into a new state description.
    class_to_new_state :: EquivalenceClass -> State NewSNum a
    class_to_new_state =
        -- A new state is constructed from any of the old states in the equivalence class.
        -- It does not matter which old state we pick since by construction of the classes
        -- they have the same behavior, both in their output (accepts) and their transitions.
        -- Since IntSet does not have a method to give an arbitrary element
        -- (ideally the one that is fastest to retrieve)
        -- we use findMin (always succeeds because the IntSet is non-empty).
        old_state_to_new_state . lookupOrPanic statemap . IntSet.findMin
      where
        lookupOrPanic = flip $ Map.findWithDefault panic
        panic = error "alex::DFAMin.minimizeDFA: panic: state not found"

    -- Convert all state numbers in the State structure to new ones.
    old_state_to_new_state :: State OldSNum a -> State NewSNum a
    old_state_to_new_state (State old_accepts old_transitions) =
      State (map fix_acc old_accepts) (fmap get_new old_transitions)

    fix_acc :: Accept a -> Accept a
    fix_acc acc = acc { accRightCtx = fmap get_new $ accRightCtx acc }

    get_new :: OldSNum -> NewSNum
    get_new k = IntMap.findWithDefault panic k old_to_new
      where
        panic = error "alex::DFAMin.minimizeDFA: panic: state not found"

    -- Memoized translation of old state numbers to new state numbers.
    old_to_new :: IntMap NewSNum
    old_to_new = IntMap.fromList $ do
      (n,ss) <- numbered_states
      s <- IntSet.toList ss
      pure (s,n)


-- | An equivalence class is a /non-empty/ set of states.
type EquivalenceClass = IntSet


-- | Creates the subsets of Q that are used to initialize W.
--
-- As per the two conditions listed in Note [Hopcroft's Algorithm], we have two
-- requirements: the union of all resulting sets must be equivalent to Q (the set
-- of all states), and all equivalent states must be in the same subsets.
--
-- We group states by their list of 'Accept'.
initialSubsets :: forall a. Ord a => DFA OldSNum a -> [EquivalenceClass]
initialSubsets dfa = Map.elems $ Map.fromListWith IntSet.union $ do
  (stateIndex, State accepts _transitions) <- Map.toList $ dfa_states dfa
  pure (accepts, IntSet.singleton stateIndex)


-- | Creates a cache of all reverse transitions for a given DFA.
--
-- To each token c in Σ, the resulting map associates a reverse map of
-- transitions. That is: for each c, we have a map that, to a state s,
-- associates the set of states that can reach s via c.
--
-- Given that the actual value of c is never actually required, we flatten the
-- result into a list.
generateReverseTransitionCache :: forall a. Ord a => DFA OldSNum a -> [IntMap EquivalenceClass]
generateReverseTransitionCache dfa = IntMap.elems $
  IntMap.fromListWith (IntMap.unionWith IntSet.union) $ do
    (sourceState, State _accepts transitions) <- Map.toList $ dfa_states dfa
    (token, targetState) <- IntMap.toList transitions
    pure (token, IntMap.singleton targetState (IntSet.singleton sourceState))


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
groupEquivalentStates :: forall a. Ord a => DFA OldSNum a -> [EquivalenceClass]
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
      let x = IntSet.unions $ do
            (target, sources) <- IntMap.toList allPreviousStates
            guard $ target `IntSet.member` a
            pure sources
      guard $ not $ IntSet.null x
      pure x

    -- Given X, refine values in R, then refine values in W, building
    -- the new values of R and W along the way.
    refineWithX (r, w) x =
      let (r', w') = List.foldl' (processR x) ([], []) r
      in  (r', List.foldl' (processW x) w' w)

    processR x (r', w') y = case refine x y of
      Nothing -> (y:r', w')
      Just (y1, y2)
        | IntSet.size y1 <= IntSet.size y2 -> (y2:r', y1:w')
        | otherwise                        -> (y1:r', y2:w')

    processW x w' y = case refine x y of
      Nothing       -> y:w'
      Just (y1, y2) -> y1:y2:w'
