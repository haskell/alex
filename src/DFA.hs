-- -----------------------------------------------------------------------------
--
-- DFA.hs, part of Alex
--
-- (c) Chris Dornan 1995-2000, Simon Marlow 2003
--
-- This module generates a DFA from a scanner by first converting it
-- to an NFA and then converting the NFA with the subset construction.
--
-- See the chapter on `Finite Automata and Lexical Analysis' in the
-- dragon book for an excellent overview of the algorithms in this
-- module.
--
-- ----------------------------------------------------------------------------}

module DFA (scanner2dfa) where

import Data.Array    ( (!) )
import Data.Function ( on )
import Data.Maybe    ( fromJust )

import qualified Data.IntMap as IntMap
import qualified Data.IntSet as IntSet
import qualified Data.Map    as Map
import qualified Data.List   as List

import AbsSyn
import NFA
import CharSet

{-                        Defined in the Scan Module

-- (This section should logically belong to the DFA module but it has been
-- placed here to make this module self-contained.)
--
-- `DFA' provides an alternative to `Scanner' (described in the RExp module);
-- it can be used directly to scan text efficiently.  Additionally it has an
-- extra place holder for holding action functions for generating
-- application-specific tokens.  When this place holder is not being used, the
-- unit type will be used.
--
-- Each state in the automaton consist of a list of `Accept' values, descending
-- in priority, and an array mapping characters to new states.  As the array
-- may only cover a sub-range of the characters, a default state number is
-- given in the third field.  By convention, all transitions to the -1 state
-- represent invalid transitions.
--
-- A list of accept states is provided for as the original specification may
-- have been ambiguous, in which case the highest priority token should be
-- taken (the one appearing earliest in the specification); this can not be
-- calculated when the DFA is generated in all cases as some of the tokens may
-- be associated with leading or trailing context or start codes.
--
-- `scan_token' (see above) can deal with unconditional accept states more
-- efficiently than those associated with context; to save it testing each time
-- whether the list of accept states contains an unconditional state, the flag
-- in the first field of `St' is set to true whenever the list contains an
-- unconditional state.
--
-- The `Accept' structure contains the priority of the token being accepted
-- (lower numbers => higher priorities), the name of the token, a place holder
-- that can be used for storing the `action' function for constructing the
-- token from the input text and the scanner's state, a list of start codes
-- (listing the start codes that the scanner must be in for the token to be
-- accepted; empty => no restriction), the leading and trailing context (both
-- `Nothing' if there is none).
--
-- The leading context consists simply of a character predicate that will
-- return true if the last character read is acceptable.  The trailing context
-- consists of an alternative starting state within the DFA; if this `sub-dfa'
-- turns up any accepting state when applied to the residual input then the
-- trailing context is acceptable (see `scan_token' above).

type DFA a = Array SNum (State a)

type SNum = Int

data State a = St Bool [Accept a] SNum (Array Char SNum)

data Accept a = Acc Int String a [StartCode] (MB(Char->Bool)) (MB SNum)

type StartCode = Int
-}


-- Scanners are converted to DFAs by converting them to NFAs first.  Converting
-- an NFA to a DFA works by identifying the states of the DFA with subsets of
-- the NFA.  The PartDFA is used to construct the DFA; it is essentially a DFA
-- in which the states are represented directly by state sets of the NFA.
-- `nfa2pdfa' constructs the partial DFA from the NFA by searching for all the
-- transitions from a given list of state sets, initially containing the start
-- state of the partial DFA, until all possible state sets have been considered
-- The final DFA is then constructed with a `mk_dfa'.

scanner2dfa:: Encoding -> Scanner -> [StartCode] -> DFA SNum Code
scanner2dfa enc scanner scs = nfa2dfa scs (scanner2nfa enc scanner scs)

nfa2dfa:: [StartCode] -> NFA -> DFA SNum Code
nfa2dfa scs nfa = mk_int_dfa nfa (nfa2pdfa nfa pdfa (dfa_start_states pdfa))
        where
        pdfa = new_pdfa n_starts nfa
        n_starts = length scs  -- number of start states

-- `nfa2pdfa' works by taking the next outstanding state set to be considered
-- and ignoring it if the state is already in the partial DFA, otherwise
-- generating all possible transitions from it, adding the new state to the
-- partial DFA and continuing the closure with the extra states.  Note the way
-- it incorporates the trailing context references into the search (by
-- including `rctx_ss' in the search).

nfa2pdfa:: NFA -> DFA StateSet Code -> [StateSet] -> DFA StateSet Code
nfa2pdfa _   pdfa [] = pdfa
nfa2pdfa nfa pdfa (ss:umkd)
  |  ss `in_pdfa` pdfa =  nfa2pdfa nfa pdfa  umkd
  |  otherwise         =  nfa2pdfa nfa pdfa' umkd'
  where
        pdfa' = add_pdfa ss (State accs (IntMap.fromList ss_outs)) pdfa

        umkd' = rctx_sss ++ map snd ss_outs ++ umkd

        -- for each character, the set of states that character would take
        -- us to from the current set of states in the NFA.
        ss_outs :: [(Int, StateSet)]
        ss_outs = [ (fromIntegral ch, mk_ss nfa ss')
                  | ch  <- byteSetElems $ setUnions [p | (p,_) <- outs],
                    let ss'  = [ s' | (p,s') <- outs, byteSetElem p ch ],
                    not (null ss')
                  ]

        rctx_sss = [ mk_ss nfa [s]
                   | Acc _ _ _ (RightContextRExp s) <- accs ]

        outs :: [(ByteSet,SNum)]
        outs =  [ out | s <- ss, out <- nst_outs (nfa ! s) ]

        accs = sort_accs [ acc | s <- ss, acc <- nst_accs (nfa ! s) ]

-- `sort_accs' sorts a list of accept values into descending order of priority,
-- eliminating any elements that follow an unconditional accept value.

sort_accs :: [Accept a] -> [Accept a]
sort_accs accs = foldr chk [] $ List.sortBy (compare `on` accPrio) accs
        where
        chk acc@(Acc _ _ Nothing NoRightContext) _   = [acc]
        chk acc                                  rst = acc:rst


{------------------------------------------------------------------------------
                          State Sets and Partial DFAs
------------------------------------------------------------------------------}



-- A `PartDFA' is a partially constructed DFA in which the states are
-- represented by sets of states of the original NFA.  It is represented by a
-- triple consisting of the start state of the partial DFA, the NFA from which
-- it is derived and a map from state sets to states of the partial DFA.  The
-- state set for a given list of NFA states is calculated by taking the epsilon
-- closure of all the states, sorting the result with duplicates eliminated.

type StateSet = [SNum]

new_pdfa :: Int -> NFA -> DFA StateSet a
new_pdfa starts nfa
 = DFA { dfa_start_states = [ List.sort $ nst_cl $ nfa ! n | n <- [0 .. starts - 1] ]
       , dfa_states       = Map.empty
       }

 -- starts is the number of start states

-- constructs the epsilon-closure of a set of NFA states
mk_ss :: NFA -> [SNum] -> StateSet
mk_ss nfa l = IntSet.toAscList $ IntSet.fromList [ s' | s <- l, s' <- nst_cl (nfa ! s) ]

add_pdfa:: StateSet -> State StateSet a -> DFA StateSet a -> DFA StateSet a
add_pdfa ss pst (DFA st mp) = DFA st (Map.insert ss pst mp)

in_pdfa:: StateSet -> DFA StateSet a -> Bool
in_pdfa ss (DFA _ mp) = ss `Map.member` mp

-- Construct a DFA with numbered states, from a DFA whose states are
-- sets of states from the original NFA.

mk_int_dfa:: NFA -> DFA StateSet a -> DFA SNum a
mk_int_dfa nfa (DFA start_states mp)
  = DFA [0 .. length start_states-1]
        (Map.fromList [ (lookup' st, cnv pds) | (st, pds) <- Map.toAscList mp ])
  where
        mp' = Map.fromList (zip (start_states ++
                                 (map fst . Map.toAscList) (foldr Map.delete mp start_states)) [0..])

        lookup' = fromJust . flip Map.lookup mp'

        cnv :: State StateSet a -> State SNum a
        cnv (State accs as) = State accs' as'
                where
                as'   = IntMap.mapWithKey (\_ch s -> lookup' s) as

                accs' = map cnv_acc accs
                cnv_acc (Acc p a lctx rctx) = Acc p a lctx rctx'
                  where rctx' =
                          case rctx of
                                RightContextRExp s ->
                                  RightContextRExp (lookup' (mk_ss nfa [s]))
                                other -> other
