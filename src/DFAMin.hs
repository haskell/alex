{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# LANGUAGE PatternGuards #-}
module DFAMin (minimizeDFA) where

import AbsSyn

import Data.Map (Map)
import qualified Data.Map as Map
import Data.IntSet (IntSet)
import qualified Data.IntSet as IS
import Data.IntMap (IntMap)
import qualified Data.IntMap as IM
import Data.List as List


-- Hopcroft's Algorithm for DFA minimization (cut/pasted from Wikipedia):

-- P := {{all accepting states}, {all nonaccepting states}};
-- Q := {{all accepting states}};
-- while (Q is not empty) do
--      choose and remove a set A from Q
--      for each c in ∑ do
--           let X be the set of states for which a transition on c leads to a state in A
--           for each set Y in P for which X ∩ Y is nonempty do
--                replace Y in P by the two sets X ∩ Y and Y \ X
--                if Y is in Q
--                     replace Y in Q by the same two sets
--                else
--                     add the smaller of the two sets to Q
--           end;
--      end;
-- end;

minimizeDFA :: Ord a => DFA Int a -> DFA Int a
minimizeDFA  dfa@ DFA { dfa_start_states = starts,
                        dfa_states       = statemap
                      }
  = DFA { dfa_start_states = starts,
          dfa_states       = Map.fromList states }
  where
      equiv_classes   = groupEquivStates dfa

      numbered_states = number (length starts) equiv_classes

      -- assign each state in the minimized DFA a number, making
      -- sure that we assign the numbers [0..] to the start states.
      number _ [] = []
      number n (ss:sss) =
        case filter (`IS.member` ss) starts of
          []      -> (n,ss) : number (n+1) sss
          starts' -> zip starts' (repeat ss) ++ number n sss
          -- if one of the states of the minimized DFA corresponds
          -- to multiple starts states, we just have to duplicate
          -- that state.

      states = [
                let old_states = map (lookup statemap) (IS.toList equiv)
                    accs = map fix_acc (state_acc (head old_states))
                           -- accepts should all be the same
                    out  = IM.fromList [ (b, get_new old)
                                           | State _ out <- old_states,
                                             (b,old) <- IM.toList out ]
                in (n, State accs out)
               | (n, equiv) <- numbered_states
               ]

      fix_acc acc = acc { accRightCtx = fix_rctxt (accRightCtx acc) }

      fix_rctxt (RightContextRExp s) = RightContextRExp (get_new s)
      fix_rctxt other = other

      lookup m k = Map.findWithDefault (error "minimizeDFA") k m
      get_new = lookup old_to_new

      old_to_new :: Map Int Int
      old_to_new = Map.fromList [ (s,n) | (n,ss) <- numbered_states,
                                          s <- IS.toList ss ]


groupEquivStates :: (Ord a) => DFA Int a -> [IntSet]
groupEquivStates DFA { dfa_states = statemap }
  = go init_p init_q
  where
    (accepting, nonaccepting) = Map.partition acc statemap
       where acc (State as _) = not (List.null as)

    nonaccepting_states = IS.fromList (Map.keys nonaccepting)

    -- group the accepting states into equivalence classes
    accept_map = {-# SCC "accept_map" #-}
      foldl' (\m (n,s) -> Map.insertWith (++) (state_acc s) [n] m)
             Map.empty
             (Map.toList accepting)

    -- accept_groups :: Ord s => [Set s]
    accept_groups = map IS.fromList (Map.elems accept_map)

    init_p = nonaccepting_states : accept_groups
    init_q = accept_groups

    -- map token T to
    --   a map from state S to the list of states that transition to
    --   S on token T
    -- This is a cache of the information needed to compute x below
    bigmap :: IntMap (IntMap [SNum])
    bigmap = IM.fromListWith (IM.unionWith (++))
                [ (i, IM.singleton to [from])
                | (from, state) <- Map.toList statemap,
                  (i,to) <- IM.toList (state_out state) ]

    -- incoming I A = the set of states that transition to a state in
    -- A on token I.
    incoming :: Int -> IntSet -> IntSet
    incoming i a = IS.fromList (concat ss)
       where
         map1 = IM.findWithDefault IM.empty i bigmap
         ss = [ IM.findWithDefault [] s map1
              | s <- IS.toList a ]

    -- The outer loop: recurse on each set in Q
    go p [] = p
    go p (a:q) = go1 0 p q
     where
       -- recurse on each token (0..255)
       go1 256 p q = go p q
       go1 i   p q = go1 (i+1) p' q'
          where
            (p',q') = go2 p [] q

            x = incoming i a

            -- recurse on each set in P
            go2 []    p' q = (p',q)
            go2 (y:p) p' q
              | IS.null i || IS.null d = go2 p (y:p') q
              | otherwise              = go2 p (i:d:p') q1
              where
                    i = IS.intersection x y
                    d = IS.difference y x

                    q1 = replaceyin q
                           where
                             replaceyin [] =
                                if IS.size i < IS.size d then [i] else [d]
                             replaceyin (z:zs)
                                | z == y    = i : d : zs
                                | otherwise = z : replaceyin zs



