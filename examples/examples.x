"example_rexps":-

  <a_star>  ::= $ | a+		-- = a*, zero or more as
  <a_plus>  ::= aa*		-- = a+, one or more as
  <a_quest> ::= $ | a		-- = a?, zero or one as
  <a_3>     ::= a{3}		-- = aaa, three as
  <a_3_5>   ::= a{3,5}		-- = a{3}a?a?
  <a_3_>    ::= a{3,}		-- = a{3}a*


"example_sets":-

  <lls>      ::= a-z		   -- little letters
  <not_lls>  ::= ~a-z		   -- anything but little letters
  <ls_ds>    ::= [a-zA-Z0-9]	   -- letters and digits
  <sym>	     ::= `!@@#$'            -- the symbols !, @@, # and $
  <sym_q_nl> ::= [`!#@@$'^'^n]	    -- the above symbols with ' and newline
  <quotable> ::= ^p#^'		   -- any graphic character except '
  <del>      ::= ^127		   -- ASCII DEL
