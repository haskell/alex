-- -----------------------------------------------------------------------------
-- 
-- NFA.hs, part of Alex
--
-- (c) Chris Dornan 1995-2000, Simon Marlow 2003
--
-- The `scanner2nfa' takes a `Scanner' (see the `RExp' module) and
-- generates its equivelent nondeterministic finite automaton.  NFAs
-- are turned into DFAs in the DFA module.
-- 
-- See the chapter on `Finite Automata and Lexical Analysis' in the
-- dragon book for an excellent overview of the algorithms in this
-- module.
--
-- ----------------------------------------------------------------------------}

module NFA where

import AbsSyn
import CharSet
import DFS ( t_close, out )
import Map ( Map )
import qualified Map hiding ( Map )
import Util ( str, space )

#if __GLASGOW_HASKELL__ < 710
import Control.Applicative ( Applicative(..) )
#endif
import Control.Monad ( forM_, zipWithM, zipWithM_, when, liftM, ap )
import Data.Array ( Array, (!), array, listArray, assocs, bounds )

-- Each state of a nondeterministic automaton contains a list of `Accept'
-- values, a list of epsilon transitions (an epsilon transition represents a
-- transition to another state that can be made without reading a character)
-- and a list of transitions qualified with a character predicate (the
-- transition can only be made to the given state on input of a character
-- permitted by the predicate).  Although a list of `Accept' values is provided
-- for, in actual fact each state will have zero or one of them (the `Maybe'
-- type is not used because the flexibility offered by the list representation
-- is useful).

type NFA = Array SNum NState

data NState = NSt {
 nst_accs :: [Accept Code],
 nst_cl   :: [SNum],
 nst_outs :: [(ByteSet,SNum)]
 }

-- Debug stuff
instance Show NState where
  showsPrec _ (NSt accs cl outs) =
    str "NSt " . shows accs . space . shows cl . space .
        shows [ (c, s) | (c,s) <- outs ]

{-                           From the Scan Module

-- The `Accept' structure contains the priority of the token being accepted
-- (lower numbers => higher priorities), the name of the token, a place holder
-- that can be used for storing the `action' function, a list of start codes
-- (listing the start codes that the scanner must be in for the token to be
-- accepted; empty => no restriction), the leading and trailing context (both
-- `Nothing' if there is none).
--  
-- The leading context consists simply of a character predicate that will
-- return true if the last character read is acceptable.  The trailing context
-- consists of an alternative starting state within the DFA; if this `sub-dfa'
-- turns up any accepting state when applied to the residual input then the
-- trailing context is acceptable.
-}


-- `scanner2nfa' takes a scanner (see the AbsSyn module) and converts it to an
-- NFA, using the NFA creation monad (see below).
--
-- We generate a start state for each startcode, with the same number
-- as that startcode, and epsilon transitions from this state to each
-- of the sub-NFAs for each of the tokens acceptable in that startcode.

scanner2nfa:: Encoding -> Scanner -> [StartCode] -> NFA
scanner2nfa enc Scanner{scannerTokens = toks} startcodes
   = runNFA enc $
        do
          -- make a start state for each start code (these will be
          -- numbered from zero).
          start_states <- sequence (replicate (length startcodes) newState)
          
          -- construct the NFA for each token
          tok_states <- zipWithM do_token toks [0..]

          -- make an epsilon edge from each state state to each
          -- token that is acceptable in that state
          zipWithM_ (tok_transitions (zip toks tok_states)) 
                startcodes start_states

        where
          do_token (RECtx _scs lctx re rctx code) prio = do
                b <- newState
                e <- newState
                rexp2nfa b e re

                rctx_e <- case rctx of
                                  NoRightContext ->
                                        return NoRightContext
                                  RightContextCode code' ->
                                        return (RightContextCode code')
                                  RightContextRExp re' -> do 
                                        r_b <- newState
                                        r_e <- newState
                                        rexp2nfa r_b r_e re'
                                        accept r_e rctxt_accept
                                        return (RightContextRExp r_b)

                let lctx' = case lctx of
                                  Nothing -> Nothing
                                  Just st -> Just st

                accept e (Acc prio code lctx' rctx_e)
                return b

          tok_transitions toks_with_states start_code start_state = do
                let states = [ s | (RECtx scs _ _ _ _, s) <- toks_with_states,
                                   null scs || start_code `elem` map snd scs ]
                mapM_ (epsilonEdge start_state) states

-- -----------------------------------------------------------------------------
-- NFA creation from a regular expression

-- rexp2nfa B E R generates an NFA that begins in state B, recognises
-- R, and ends in state E only if R has been recognised. 

rexp2nfa :: SNum -> SNum -> RExp -> NFAM ()
rexp2nfa b e Eps    = epsilonEdge b e
rexp2nfa b e (Ch p) = charEdge b p e
rexp2nfa b e (re1 :%% re2) = do
  s <- newState
  rexp2nfa b s re1
  rexp2nfa s e re2
rexp2nfa b e (re1 :| re2) = do
  rexp2nfa b e re1
  rexp2nfa b e re2
rexp2nfa b e (Star re) = do
  s <- newState
  epsilonEdge b s
  rexp2nfa s s re
  epsilonEdge s e
rexp2nfa b e (Plus re) = do
  s1 <- newState
  s2 <- newState
  rexp2nfa s1 s2 re
  epsilonEdge b s1
  epsilonEdge s2 s1
  epsilonEdge s2 e
rexp2nfa b e (Ques re) = do
  rexp2nfa b e re
  epsilonEdge b e

-- -----------------------------------------------------------------------------
-- NFA creation monad.

-- Partial credit to Thomas Hallgren for this code, as I adapted it from
-- his "Lexing Haskell in Haskell" lexer generator.

type MapNFA = Map SNum NState

newtype NFAM a = N {unN :: SNum -> MapNFA -> Encoding -> (SNum, MapNFA, a)}

instance Functor NFAM where
  fmap = liftM

instance Applicative NFAM where
  pure a = N $ \s n _ -> (s,n,a)
  (<*>) = ap

instance Monad NFAM where
  return = pure

  m >>= k  = N $ \s n e -> case unN m s n e of
                                 (s', n', a) -> unN (k a) s' n' e

runNFA :: Encoding -> NFAM () -> NFA
runNFA e m = case unN m 0 Map.empty e of
                (s, nfa_map, ()) -> -- trace ("runNfa.." ++ show (Map.toAscList nfa_map)) $ 
                                    e_close (array (0,s-1) (Map.toAscList nfa_map))

e_close:: Array Int NState -> NFA
e_close ar = listArray bds
                [NSt accs (out gr v) outs|(v,NSt accs _ outs)<-assocs ar]
        where
        gr = t_close (hi+1,\v->nst_cl (ar!v))
        bds@(_,hi) = bounds ar

newState :: NFAM SNum
newState = N $ \s n _ -> (s+1,n,s)

getEncoding :: NFAM Encoding
getEncoding = N $ \s n e -> (s,n,e)

anyBytes :: SNum -> Int -> SNum -> NFAM ()
anyBytes from 0 to = epsilonEdge from to
anyBytes from n to = do
        s <- newState
        byteEdge from (byteSetRange 0 0xff) s
        anyBytes s (n-1) to

bytesEdge :: SNum -> [Byte] -> [Byte] -> SNum -> NFAM ()
bytesEdge from [] [] to = epsilonEdge from to
bytesEdge from [x] [y] to = byteEdge from (byteSetRange x y) to -- (OPTIMISATION)
bytesEdge from (x:xs) (y:ys) to 
    | x == y = do 
        s <- newState
        byteEdge from (byteSetSingleton x) s
        bytesEdge s xs ys to
    | x < y = do
        do s <- newState
           byteEdge from (byteSetSingleton x) s
           bytesEdge s xs (fmap (const 0xff) ys) to

        do t <- newState
           byteEdge from (byteSetSingleton y) t
           bytesEdge t (fmap (const 0x00) xs) ys to

        when ((x+1) <= (y-1)) $ do 
           u <- newState
           byteEdge from (byteSetRange (x+1) (y-1)) u
           anyBytes u (length xs) to
bytesEdge _ _ _ _ = undefined -- hide compiler warning

charEdge :: SNum -> CharSet -> SNum -> NFAM ()
charEdge from charset to = do
  -- trace ("charEdge: " ++ (show $ charset) ++ " => " ++ show (byteRanges charset)) $ 
  e <- getEncoding
  forM_ (byteRanges e charset) $ \(xs,ys) -> do
    bytesEdge from xs ys to
    


byteEdge :: SNum -> ByteSet -> SNum -> NFAM ()
byteEdge from charset to = N $ \s n _ -> (s, addEdge n, ())
 where
   addEdge n =
     case Map.lookup from n of
       Nothing -> 
           Map.insert from (NSt [] [] [(charset,to)]) n
       Just (NSt acc eps trans) ->
           Map.insert from (NSt acc eps ((charset,to):trans)) n

epsilonEdge :: SNum -> SNum -> NFAM ()
epsilonEdge from to 
 | from == to = return ()
 | otherwise  = N $ \s n _ -> let n' = addEdge n in n' `seq` (s, n', ())
 where
   addEdge n =
     case Map.lookup from n of
       Nothing                  -> Map.insert from (NSt [] [to] []) n
       Just (NSt acc eps trans) -> Map.insert from (NSt acc (to:eps) trans) n

accept :: SNum -> Accept Code -> NFAM ()
accept state new_acc = N $ \s n _ -> (s, addAccept n, ())
 where
   addAccept n = 
     case Map.lookup state n of
       Nothing ->
           Map.insert state (NSt [new_acc] [] []) n
       Just (NSt acc eps trans) ->
           Map.insert state (NSt (new_acc:acc) eps trans) n


rctxt_accept :: Accept Code
rctxt_accept = Acc 0 Nothing Nothing NoRightContext
