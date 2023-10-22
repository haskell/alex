{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

{-# LANGUAGE CPP #-}

module DFAMin (minimizeDFA) where

import AbsSyn

import Data.IntMap ( IntMap )
import Data.IntSet ( IntSet )
import Data.Map    ( Map )
#if !MIN_VERSION_containers(0,6,0)
import Data.Maybe  ( mapMaybe )
#endif

import qualified Data.Map    as Map
import qualified Data.IntSet as IntSet
import qualified Data.IntMap as IntMap
import qualified Data.List   as List

-- % Hopcroft's Algorithm for DFA minimization (cut/pasted from Wikipedia):
-- % X refines Y into Y1 and Y2 means
-- %  Y1 := Y ∩ X
-- %  Y2 := Y \ X
-- %  where both Y1 and Y2 are nonempty
--
-- P := {{all accepting states}, {all nonaccepting states}};
-- Q := {{all accepting states}};
-- while (Q is not empty) do
--      choose and remove a set A from Q
--      for each c in ∑ do
--           let X be the set of states for which a transition on c leads to a state in A
--           for each set Y in P for which X refines Y into Y1 and Y2 do
--                replace Y in P by the two sets Y1 and Y2
--                if Y is in Q
--                     replace Y in Q by the same two sets
--                else
--                     add the smaller of the two sets to Q
--           end;
--      end;
-- end;
--
-- % X is a preimage of A under transition function.

-- % observation : Q is always subset of P
-- % let R = P \ Q. then following algorithm is the equivalent of the Hopcroft's Algorithm
--
-- R := {{all nonaccepting states}};
-- Q := {{all accepting states}};
-- while (Q is not empty) do
--      choose a set A from Q
--      remove A from Q and add it to R
--      for each c in ∑ do
--           let X be the set of states for which a transition on c leads to a state in A
--           for each set Y in R for which X refines Y into Y1 and Y2 do
--                replace Y in R by the greater of the two sets Y1 and Y2
--                add the smaller of the two sets to Q
--           end;
--           for each set Y in Q for which X refines Y into Y1 and Y2 do
--                replace Y in Q by the two sets Y1 and Y2
--           end;
--      end;
-- end;
--
-- % The second for loop that iterates over R mutates Q,
-- % but it does not affect the third for loop that iterates over Q.
-- % Because once X refines Y into Y1 and Y2, Y1 and Y2 can't be more refined by X.

minimizeDFA :: forall a. Ord a => DFA Int a -> DFA Int a
minimizeDFA  dfa@(DFA { dfa_start_states = starts,
                        dfa_states       = statemap
                      })
  = DFA { dfa_start_states = starts,
          dfa_states       = Map.fromList states }
  where
      equiv_classes   :: [EquivalenceClass]
      equiv_classes   = groupEquivStates dfa

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
      states = [
                let old_states = map (lookup statemap) (IntSet.toList equiv)
                    accs = map fix_acc (state_acc (headWithDefault undefined old_states))
                           -- accepts should all be the same
                    out  = IntMap.fromList [ (b, get_new old)
                                           | State _ out <- old_states,
                                             (b,old) <- IntMap.toList out ]
                in (n, State accs out)
               | (n, equiv) <- numbered_states
               ]

      fix_acc :: Accept a -> Accept a
      fix_acc acc = acc { accRightCtx = fix_rctxt (accRightCtx acc) }

      fix_rctxt :: RightContext SNum -> RightContext SNum
      fix_rctxt (RightContextRExp s) = RightContextRExp (get_new s)
      fix_rctxt other = other

      lookup :: Ord k => Map k v -> k -> v
      lookup m k = Map.findWithDefault (error "minimizeDFA") k m

      get_new :: Int -> Int
      get_new = lookup old_to_new

      old_to_new :: Map Int Int
      old_to_new = Map.fromList [ (s,n) | (n,ss) <- numbered_states,
                                          s <- IntSet.toList ss ]

type EquivalenceClass = IntSet

groupEquivStates :: forall a. Ord a => DFA Int a -> [EquivalenceClass]
groupEquivStates DFA { dfa_states = statemap }
  = go init_r init_q
  where
    accepting, nonaccepting :: Map Int (State Int a)
    (accepting, nonaccepting) = Map.partition acc statemap
       where acc (State as _) = not (List.null as)

    nonaccepting_states :: EquivalenceClass
    nonaccepting_states = IntSet.fromList (Map.keys nonaccepting)

    -- group the accepting states into equivalence classes
    accept_map :: Map [Accept a] [Int]
    accept_map = {-# SCC "accept_map" #-}
      List.foldl' (\m (n,s) -> Map.insertWith (++) (state_acc s) [n] m)
             Map.empty
             (Map.toList accepting)

    accept_groups :: [EquivalenceClass]
    accept_groups = map IntSet.fromList (Map.elems accept_map)

    init_r, init_q :: [EquivalenceClass]
    init_r  -- Issue #71: each EquivalenceClass needs to be a non-empty set
      | IntSet.null nonaccepting_states = []
      | otherwise                   = [nonaccepting_states]
    init_q = accept_groups

    -- a map from token T to
    --   a map from state S to the set of states that transition to
    --   S on token T
    -- bigmap is an inversed transition function classified by each input token.
    -- the codomain of each inversed function is a set of states rather than single state
    -- since a transition function might not be an injective.
    -- This is a cache of the information needed to compute xs below
    bigmap :: IntMap (IntMap EquivalenceClass)
    bigmap = IntMap.fromListWith (IntMap.unionWith IntSet.union)
                [ (i, IntMap.singleton to (IntSet.singleton from))
                | (from, state) <- Map.toList statemap,
                  (i,to) <- IntMap.toList (state_out state) ]

    -- The outer loop: recurse on each set in R and Q
    go :: [EquivalenceClass] -> [EquivalenceClass] -> [EquivalenceClass]
    go r [] = r
    go r (a:q) = uncurry go $ List.foldl' go0 (a:r,q) xs
      where
        preimage :: IntMap EquivalenceClass -- inversed transition function
                 -> EquivalenceClass        -- subset of codomain of original transition function
                 -> EquivalenceClass        -- preimage of given subset
#if MIN_VERSION_containers(0,6,0)
        preimage invMap = IntSet.unions . IntMap.restrictKeys invMap
#else
        preimage invMap = IntSet.unions . mapMaybe (`IntMap.lookup` invMap) . IntSet.toList
#endif

        xs :: [EquivalenceClass]
        xs =
          [ x
          | invMap <- IntMap.elems bigmap
          , let x = preimage invMap a
          , not (IntSet.null x)
          ]

        refineWith
          :: EquivalenceClass -- preimage set that bisects the input equivalence class
          -> EquivalenceClass -- input equivalence class
          -> Maybe (EquivalenceClass, EquivalenceClass) -- refined equivalence class
        refineWith x y =
          if IntSet.null y1 || IntSet.null y2
            then Nothing
            else Just (y1, y2)
          where
            y1 = IntSet.intersection y x
            y2 = IntSet.difference   y x

        go0 (r,q) x = go1 r [] []
          where
            -- iterates over R
            go1 []    r' q' = (r', go2 q q')
            go1 (y:r) r' q' = case refineWith x y of
              Nothing                       -> go1 r (y:r') q'
              Just (y1, y2)
                | IntSet.size y1 <= IntSet.size y2 -> go1 r (y2:r') (y1:q')
                | otherwise                        -> go1 r (y1:r') (y2:q')

            -- iterates over Q
            go2 []    q' = q'
            go2 (y:q) q' = case refineWith x y of
              Nothing       -> go2 q (y:q')
              Just (y1, y2) -> go2 q (y1:y2:q')

-- To pacify GHC 9.8's warning about 'head'
headWithDefault :: a -> [a] -> a
headWithDefault a []    = a
headWithDefault _ (a:_) = a
