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
import Util

import Data.FiniteMap
import Data.Array

-- -----------------------------------------------------------------------------
-- Generate a human readable dump of the state machine

infoDFA :: Int -> String -> DFA SNum Code -> ShowS
infoDFA n func_nm dfa
  = str "Scanner : " . str func_nm . nl
  . str "States  : " . shows (length dfa_list) . nl
  . nl . infoDFA dfa
  where    
    dfa_list = fmToList (dfa_states dfa)

    infoDFA dfa = interleave_shows nl (map infoStateN dfa_list)

    infoStateN (i,s) = str "State " . shows i . nl . infoState s . nl

    infoState :: State SNum Code -> ShowS
    infoState (State accs out)
	= infoArr out . nl
	-- . str ("\tDefault -> ") . shows df

    infoArr out
	= char '\t' . interleave_shows (str "\n\t")
			(map infoTransition (fmToList out))

    infoTransition (char,state)
	= str (ljustify 8 (show char))
	. str " -> "
	. shows state

--    outputAccs :: [Accept Code] -> ShowS
--    outputAccs accs
--	  = brack (interleave_shows (char ',') (map (paren.outputAcc) accs))
--   
--    outputAcc (Acc prio act scs lctx rctx)
--	  = str "Acc " . shows prio . space
--	  . paren (str act) . space
--	  . shows scs . space
--	  . outputLCtx lctx . space
--	  . shows rctx
--
--    outputLCtx Nothing
--	  = str "Nothing"
--    outputLCtx (Just set)
--	  = str "Just " . paren (outputArr (charSetToArray set))
--
--    outputArr arr
--	  = str "Array.array " . shows (bounds arr) . space
--	  . shows (assocs arr)
