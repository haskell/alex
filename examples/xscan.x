%{
import Char
import Alex
%}



"x_lx"/"x_acts":-

{ ^l = [a-zA-Z]			 }     	  -- letters
{ ^d = 0-9			 }	  -- digits
{ ^i = [^l^_^d^']		 }	  -- identifier character
{ ^s = `!#$%&*+-./:<=>?@\\^|~'   }    	  -- symbolic char
{ %e = ^\([abfnrtv^"^\]|^d{1,3}) } 	  -- character escape
{ %c = ^p#[^"^\]|%e		 }	  -- string character
{ %g = ^\^w+^\			 }     	  -- string gap

  <>      ::= ^w+			  -- white space
  <>      ::= ^-^-.*			  -- line comment
  <sym>   ::= `,()[]_{;}'		  -- single char token
  <dd>    ::= ^.^.			  -- ..
  <ig>    ::= ^-?^d+			  -- signed integer literal
  <oig>   ::= ^-?0[oO][0-7]+		  -- signed octal literal
  <hig>   ::= ^-?0[xX][0-9a-fA-F]+	  -- signed hexadecimal literal
  <ch>    ::= ^'(%c|^")^'		  -- character literal
  <str>   ::= ^"(%c|%g)*^"		  -- string literal
  <ide>   ::= ^l^i*                       -- alphabetic identifier
  <sid>   ::= ^s+			  -- symbolic identifier



%{
sym = mtk (\s->SymT(head s))
dd  = mtk (\s->DotDotT)
ig  = mtk cnv_int
oig = mtk cnv_oint
hig = mtk cnv_hint
ch  = mtk cnv_char
str = mtk cnv_str
ide = mtk (IdeT False)
sid = mtk (IdeT True)

data Token = Tk Posn Tkn			deriving (Eq,Show)

data Tkn = 	SymT    Char 	    |
		DotDotT 	    |
		IntT    Int 	    |
		CharT   Char 	    |
		StringT String 	    |
		IdeT    Bool String |
		ErrorT
						deriving (Eq,Show)

main :: IO ()
main = interact (show.scan_x)

test :: String -> IO ()
test fn = do
	cts <- readFile fn
	print (scan_x cts)

scan_x:: String -> [Token]
scan_x inp = gscan x_scan () inp

x_scan:: GScan () [Token]
x_scan = load_gscan (x_acts,stop_act) x_lx
	where
	stop_act p _ "" _ = []
	stop_act p _ _  _ = [Tk p ErrorT]


mtk:: (String->Tkn) -> GTokenAction () [Token]
mtk f p _ str n cont st = Tk p (f(take n str)):cont st


cnv_int:: String -> Tkn
cnv_int ('-':str) = IntT (-parse_int 10 str)
cnv_int str = IntT (parse_int 10 str)

cnv_oint:: String -> Tkn
cnv_oint ('-':str) = IntT (-parse_int 8 (drop 2 str))
cnv_oint str = IntT (parse_int 8 (drop 2 str))

cnv_hint:: String -> Tkn
cnv_hint ('-':str) = IntT (-parse_int 16 (drop 2 str))
cnv_hint str = IntT (parse_int 16 (drop 2 str))

parse_int:: Int -> String -> Int
parse_int bse str = foldl (\n d-> n*bse+cnv d) 0 str
	where
	cnv c =	if isLower c then
			10+ord c-ord 'a'
		else if isUpper c then
			10+ord c-ord 'A'
		else
			ord c-ord '0'
		

cnv_char:: String -> Tkn
cnv_char str = CharT (fst(cnv_ch(tail str)))

cnv_str:: String -> Tkn
cnv_str str0 = StringT (c_s(tail str0))		-- ignore initial `"'
	where
	c_s str =
		case cnv_ch str of
		  (_,[]) -> []			-- ignore final `"'
		  (c,rst@(_:_)) -> c:c_s rst

cnv_ch:: String -> (Char,String)
cnv_ch "" = error "cnv_ch"
cnv_ch (c:rst) =
	case c of
	  '\\' -> cnv_esc rst
	  _ -> (c,rst)
	where
	cnv_esc [] = error "cnv_ch (a)"
	cnv_esc (c:rst)
		| isSpace c = cnv_ch (tail(dropWhile ('\\' /=)  rst))
		| isDigit c = cnv_code 1 [c] rst
		| otherwise =
			case c of
			  'a' -> ('\a',rst)
			  'b' -> ('\b',rst)
			  'f' -> ('\f',rst)
			  'n' -> ('\n',rst)
			  'r' -> ('\r',rst)
			  't' -> ('\t',rst)
			  'v' -> ('\v',rst)
			  _   -> (c,rst)

	cnv_code n cd [] = (chr(parse_int 10 cd `mod` 256),[])
	cnv_code n cd (c:rst) =
			if n<3 && isDigit c
			   then cnv_code (n+1) (cd++[c]) rst
			   else (chr(parse_int 10 cd `mod` 256),c:rst)

%}
