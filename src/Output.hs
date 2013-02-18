-- -----------------------------------------------------------------------------
-- 
-- Output.hs, part of Alex
--
-- (c) Simon Marlow 2003
--
-- Code-outputing and table-generation routines
--
-- ----------------------------------------------------------------------------}

module Output (outputDFA) where

import AbsSyn
import CharSet
import Util
import qualified Map
import qualified Data.IntMap as IntMap

import Control.Monad.ST ( ST, runST )
import Data.Array ( Array )
import Data.Array.Base ( unsafeRead )
import Data.Array.ST ( STUArray, newArray, readArray, writeArray, freeze )
import Data.Array.Unboxed ( UArray, elems, (!), array, listArray )
import Data.Bits
import Data.Char ( ord, chr )
import Data.List ( maximumBy, sortBy, groupBy )

-- -----------------------------------------------------------------------------
-- Printing the output

outputDFA :: Target -> Int -> String -> DFA SNum Code -> ShowS
outputDFA target _ _ dfa
  = interleave_shows nl 
	[outputBase, outputTable, outputCheck, outputDefault, outputAccept]
  where    
    (base, table, check, deflt, accept) = mkTables dfa

    table_size = length table - 1
    n_states   = length base - 1

    base_nm   = "alex_base"
    table_nm  = "alex_table"
    check_nm  = "alex_check"
    deflt_nm  = "alex_deflt"
    accept_nm = "alex_accept"

    outputBase    = do_array hexChars32 base_nm  n_states   base
    outputTable   = do_array hexChars16 table_nm table_size table
    outputCheck   = do_array hexChars16 check_nm table_size check
    outputDefault = do_array hexChars16 deflt_nm n_states   deflt

    do_array hex_chars nm upper_bound ints = -- trace ("do_array: " ++ nm) $ 
     case target of
      GhcTarget ->
	  str nm . str " :: AlexAddr\n"
	. str nm . str " = AlexA# \""
	. str (hex_chars ints)
	. str "\"#\n"

      _ ->
	  str nm . str " :: Array Int Int\n"
	. str nm . str " = listArray (0," . shows upper_bound
	. str ") [" . interleave_shows (char ',') (map shows ints)
	. str "]\n"

    outputAccept
	= -- No type signature: we don't know what the type of the actions is.
	  -- str accept_nm . str " :: Array Int (Accept Code)\n"
	  str accept_nm . str " = listArray (0::Int," . shows n_states
	. str ") [" . interleave_shows (char ',') (map outputAccs accept)
	. str "]\n"

    outputAccs :: [Accept Code] -> ShowS
    outputAccs []
	= str "AlexAccNone"
    outputAccs (Acc _ Nothing Nothing NoRightContext : [])
	= str "AlexAccSkip"
    outputAccs (Acc _ (Just act) Nothing NoRightContext : [])
	= str "AlexAcc " . paren (str act)
    outputAccs (Acc _ Nothing lctx rctx : rest)
	= str "AlexAccSkipPred " . space
	. paren (outputPred lctx rctx)
        . paren (outputAccs rest)
    outputAccs (Acc _ (Just act) lctx rctx : rest)
	= str "AlexAccPred " . space
	. paren (str act) . space
	. paren (outputPred lctx rctx)
        . paren (outputAccs rest)

    outputPred (Just set) NoRightContext
	= outputLCtx set
    outputPred Nothing rctx
	= outputRCtx rctx
    outputPred (Just set) rctx
	= outputLCtx set
	. str " `alexAndPred` "
	. outputRCtx rctx

    outputLCtx set = str "alexPrevCharMatches" . str (charSetQuote set)

    outputRCtx NoRightContext = id
    outputRCtx (RightContextRExp sn)
	= str "alexRightContext " . shows sn
    outputRCtx (RightContextCode code)
	= str code

    -- outputArr arr
	-- = str "array " . shows (bounds arr) . space
	-- . shows (assocs arr)

-- -----------------------------------------------------------------------------
-- Generating arrays.

-- Here we use the table-compression algorithm described in section
-- 3.9 of the dragon book, which is a common technique used by lexical
-- analyser generators.

-- We want to generate:
--
--    base :: Array SNum Int
--		maps the current state to an offset in the main table
--
--    table :: Array Int SNum
--		maps (base!state + char) to the next state
--
--    check :: Array Int SNum
--		maps (base!state + char) to state if table entry is valid,
--		otherwise we use the default for this state
--
--    default :: Array SNum SNum
--		default production for this state
--
--    accept :: Array SNum [Accept Code]
--		maps state to list of accept codes for this state
--
-- For each state, we decide what will be the default symbol (pick the
-- most common).  We now have a mapping Char -> SNum, with one special
-- state reserved as the default.


mkTables :: DFA SNum Code
	 -> ( 
	      [Int],		-- base
	      [Int],		-- table
	      [Int],		-- check
	      [Int],		-- default
	      [[Accept Code]]	-- accept
	    )
mkTables dfa = -- trace (show (defaults)) $
               -- trace (show (fmap (length . snd)  dfa_no_defaults)) $
  ( elems base_offs, 
     take max_off (elems table),
     take max_off (elems check),
     elems defaults,
     accept
  )
 where 
	accept   = [ as | State as _ <- elems dfa_arr ]

	state_assocs = Map.toAscList (dfa_states dfa)
	n_states = length state_assocs
	top_state = n_states - 1

	dfa_arr :: Array SNum (State SNum Code)
	dfa_arr = array (0,top_state) state_assocs

	-- fill in all the error productions
	expand_states =
	   [ expand (dfa_arr!state) | state <- [0..top_state] ]
	 
	expand (State _ out) = 
	   [(i, lookup' out i) | i <- [0..0xff]]
           where lookup' out' i = case IntMap.lookup i out' of
					Nothing -> -1
					Just s  -> s

	defaults :: UArray SNum SNum
	defaults = listArray (0,top_state) (map best_default expand_states)

	-- find the most common destination state in a given state, and
	-- make it the default.
        best_default :: [(Int,SNum)] -> SNum
	best_default prod_list
	   | null sorted = -1
	   | otherwise   = snd (head (maximumBy lengths eq))
	   where sorted  = sortBy compareSnds prod_list
		 compareSnds (_,a) (_,b) = compare a b
		 eq = groupBy (\(_,a) (_,b) -> a == b) sorted
		 lengths  a b = length a `compare` length b

	-- remove all the default productions from the DFA
        dfa_no_defaults =
	  [ (s, prods_without_defaults s out)
	  | (s, out) <- zip [0..] expand_states
	  ]

	prods_without_defaults s out 
	  = [ (fromIntegral c, dest) | (c,dest) <- out, dest /= defaults!s ]

	(base_offs, table, check, max_off)
	   = runST (genTables n_states 255 dfa_no_defaults)
	  

genTables
	 :: Int				-- number of states
	 -> Int				-- maximum token no.
	 -> [(SNum,[(Int,SNum)])]	-- entries for the table
	 -> ST s (UArray Int Int,	-- base
		  UArray Int Int,	-- table
		  UArray Int Int,	-- check
		  Int 	   		-- highest offset in table
	    )

genTables n_states max_token entries = do

  base       <- newArray (0, n_states-1) 0
  table      <- newArray (0, mAX_TABLE_SIZE) 0
  check      <- newArray (0, mAX_TABLE_SIZE) (-1)
  off_arr    <- newArray (-max_token, mAX_TABLE_SIZE) 0

  max_off    <- genTables' base table check off_arr entries max_token

  base'      <- freeze base
  table'     <- freeze table
  check'     <- freeze check
  return (base', table',check',max_off+1)

  where mAX_TABLE_SIZE = n_states * (max_token + 1)


genTables'
	 :: STUArray s Int Int		-- base
	 -> STUArray s Int Int		-- table
	 -> STUArray s Int Int		-- check
	 -> STUArray s Int Int		-- offset array
	 -> [(SNum,[(Int,SNum)])]	-- entries for the table
	 -> Int				-- maximum token no.
	 -> ST s Int 	   		-- highest offset in table

genTables' base table check off_arr entries max_token
	= fit_all entries 0 1
  where

	 fit_all [] max_off _ = return max_off
	 fit_all (s:ss) max_off fst_zero = do
	   (off, new_max_off, new_fst_zero) <- fit s max_off fst_zero
	   writeArray off_arr off 1
	   fit_all ss new_max_off new_fst_zero

	 -- fit a vector into the table.  Return the offset of the vector,
	 -- the maximum offset used in the table, and the offset of the first
	 -- entry in the table (used to speed up the lookups a bit).
	 fit (_,[]) max_off fst_zero = return (0,max_off,fst_zero)

	 fit (state_no, state@((t,_):_)) max_off fst_zero = do
		 -- start at offset 1 in the table: all the empty states
		 -- (states with just a default reduction) are mapped to
		 -- offset zero.
	   off <- findFreeOffset (-t + fst_zero) check off_arr state
	   let new_max_off | furthest_right > max_off = furthest_right
			   | otherwise                = max_off
	       furthest_right = off + max_token

 	   --trace ("fit: state " ++ show state_no ++ ", off " ++ show off ++ ", elems " ++ show state) $ do

	   writeArray base state_no off
	   addState off table check state
	   new_fst_zero <- findFstFreeSlot check fst_zero
	   return (off, new_max_off, new_fst_zero)


-- Find a valid offset in the table for this state.
findFreeOffset :: Int
               -> STUArray s Int Int
               -> STUArray s Int Int
               -> [(Int, Int)]
               -> ST s Int
findFreeOffset off check off_arr state = do
    -- offset 0 isn't allowed
  if off == 0 then try_next else do

    -- don't use an offset we've used before
  b <- readArray off_arr off
  if b /= 0 then try_next else do

    -- check whether the actions for this state fit in the table
  ok <- fits off state check
  if ok then return off else try_next 
 where
	try_next = findFreeOffset (off+1) check off_arr state

-- This is an inner loop, so we use some strictness hacks, and avoid
-- array bounds checks (unsafeRead instead of readArray) to speed
-- things up a bit.
fits :: Int -> [(Int,Int)] -> STUArray s Int Int -> ST s Bool
fits off [] check = off `seq` check `seq` return True -- strictness hacks
fits off ((t,_):rest) check = do
  i <- unsafeRead check (off+t)
  if i /= -1 then return False
	     else fits off rest check

addState :: Int -> STUArray s Int Int -> STUArray s Int Int -> [(Int, Int)]
         -> ST s ()
addState _   _     _     [] = return ()
addState off table check ((t,val):state) = do
   writeArray table (off+t) val
   writeArray check (off+t) t
   addState off table check state

findFstFreeSlot :: STUArray s Int Int -> Int -> ST s Int
findFstFreeSlot table n = do
	 i <- readArray table n
	 if i == -1 then return n
		    else findFstFreeSlot table (n+1)

-----------------------------------------------------------------------------
-- Convert an integer to a 16-bit number encoded in \xNN\xNN format suitable
-- for placing in a string (copied from Happy's ProduceCode.lhs)

hexChars16 :: [Int] -> String
hexChars16 acts = concat (map conv16 acts)
  where
    conv16 i | i > 0x7fff || i < -0x8000
    		= error ("Internal error: hexChars16: out of range: " ++ show i)
  	     | otherwise
	        = hexChar16 i

hexChars32 :: [Int] -> String
hexChars32 acts = concat (map conv32 acts)
  where
    conv32 i = hexChar16 (i .&. 0xffff) ++ 
		hexChar16 ((i `shiftR` 16) .&. 0xffff)

hexChar16 :: Int -> String
hexChar16 i = toHex (i .&. 0xff)
		 ++ toHex ((i `shiftR` 8) .&. 0xff)  -- force little-endian

toHex :: Int -> String
toHex i = ['\\','x', hexDig (i `div` 16), hexDig (i `mod` 16)]

hexDig :: Int -> Char
hexDig i | i <= 9    = chr (i + ord '0')
	 | otherwise = chr (i - 10 + ord 'a')
