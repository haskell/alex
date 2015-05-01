-- -----------------------------------------------------------------------------
-- 
-- Info.hs, part of Alex
--
-- (c) Simon Marlow 2003
--
-- Generate a human-readable rendition of the state machine.
--
-- ----------------------------------------------------------------------------}

module Info (infoDFA) where

import AbsSyn
import qualified Map
import qualified Data.IntMap as IntMap
import Util

-- -----------------------------------------------------------------------------
-- Generate a human readable dump of the state machine

infoDFA :: Int -> String -> DFA SNum Code -> ShowS
infoDFA _ func_nm dfa
  = str "Scanner : " . str func_nm . nl
  . str "States  : " . shows (length dfa_list) . nl
  . nl . infoDFA'
  where    
    dfa_list = Map.toAscList (dfa_states dfa)

    infoDFA' = interleave_shows nl (map infoStateN dfa_list)

    infoStateN (i,s) = str "State " . shows i . nl . infoState s

    infoState :: State SNum Code -> ShowS
    infoState (State accs out)
        = foldr (.) id (map infoAccept accs)
        . infoArr out . nl

    infoArr out
        = char '\t' . interleave_shows (str "\n\t")
                        (map infoTransition (IntMap.toAscList out))

    infoAccept (Acc p act lctx rctx)
        = str "\tAccept" . paren (shows p) . space
        . outputLCtx lctx . space
        . showRCtx rctx
        . (case act of
            Nothing   -> id
            Just code -> str " { " . str code . str " }")
        . nl
        
    infoTransition (char',state)
        = str (ljustify 8 (show char'))
        . str " -> "
        . shows state

    outputLCtx Nothing
          = id
    outputLCtx (Just set)
          = paren (show set ++) . char '^'

    -- outputArr arr
          -- = str "Array.array " . shows (bounds arr) . space
          -- . shows (assocs arr)
