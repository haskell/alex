%{
import System
import Char
import Alex
%}


"pp_lx"/"pp_acts":-

{ ^s = ^w#^n                }		-- spaces and tabs, etc.
{ ^f = [A-Za-z0-9`~%-_.,/'] }		-- file name character

  <inc> ::= ^#include^s+^"^f+^"^s*^n
  <txt> ::= .*^n


%{
inc p c inp len cont st = pp fn >> cont st
	where
	fn = (takeWhile ('"'/=) . tail . dropWhile isSpace . drop 8) inp

txt p c inp len cont st = putStr (take len inp) >> cont st


main:: IO ()
main =	getArgs						>>= \args ->
	case args of
	  [fn] -> pp fn
	  _ -> error "usage: pp file\n"
	
pp:: String -> IO ()
pp fn = readFile fn >>= \cts -> gscan pp_scan () cts

pp_scan:: GScan () (IO ())
pp_scan = load_gscan (pp_acts,stop_act) pp_lx
	where
	stop_act _ _ _ _ = return ()
%}
