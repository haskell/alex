-- -----------------------------------------------------------------------------
-- The input type

type AlexInput = (AlexPosn, 	-- current position,
		  Char,		-- previous char
		  String)	-- current input string

alexInputPrevChar :: AlexInput -> Char
alexInputPrevChar (p,c,s) = c

alexGetChar :: AlexInput -> Maybe (Char,AlexInput)
alexGetChar (p,c,[]) = Nothing
alexGetChar (p,_,(c:s))  = let p' = alexMove p c in p' `seq`
				Just (c, (p', c, s))

-- -----------------------------------------------------------------------------
-- Token positions

-- `Posn' records the location of a token in the input text.  It has three
-- fields: the address (number of chacaters preceding the token), line number
-- and column of a token within the file. `start_pos' gives the position of the
-- start of the file and `eof_pos' a standard encoding for the end of file.
-- `move_pos' calculates the new position after traversing a given character,
-- assuming the usual eight character tab stops.

data AlexPosn = AlexPn !Int !Int !Int
	deriving (Eq,Show)

alexStartPos :: AlexPosn
alexStartPos = AlexPn 0 1 1

alexMove :: AlexPosn -> Char -> AlexPosn
alexMove (AlexPn a l c) '\t' = AlexPn (a+1)  l     (((c+7) `div` 8)*8+1)
alexMove (AlexPn a l c) '\n' = AlexPn (a+1) (l+1)   1
alexMove (AlexPn a l c) _    = AlexPn (a+1)  l     (c+1)

-- -----------------------------------------------------------------------------
-- Default monad

data AlexState = AlexState {
	alex_pos :: !AlexPosn,	-- position at current input location
	alex_inp :: String,	-- the current input
	alex_chr :: !Char,	-- the character before the input
	alex_scd :: !Int 	-- the current startcode
    }

-- Compile with -funbox-strict-fields for best results!

#ifdef ALEX_MONAD
runAlex :: String -> Alex a -> Either String a
runAlex input (Alex f) 
   = case f (AlexState {alex_pos = alexStartPos,
 			alex_inp = input,	
			alex_chr = '\n',
			alex_scd = 0}) of Left msg -> Left msg
					  Right ( _, a ) -> Right a

newtype Alex a = Alex { unAlex :: AlexState -> Either String (AlexState, a) }

instance Monad Alex where
  m >>= k  = Alex $ \s -> case unAlex m s of 
				Left msg -> Left msg
				Right (s',a) -> unAlex (k a) s'
  return a = Alex $ \s -> Right (s,a)

alexGetInput :: Alex AlexInput
alexGetInput
 = Alex $ \s@AlexState{alex_pos=pos,alex_chr=c,alex_inp=inp} -> 
	Right (s, (pos,c,inp))

alexSetInput :: AlexInput -> Alex ()
alexSetInput (pos,c,inp)
 = Alex $ \s -> case s{alex_pos=pos,alex_chr=c,alex_inp=inp} of
		  s@(AlexState{}) -> Right (s, ())

alexError :: String -> Alex a
alexError message = Alex $ \s -> Left message

alexGetStartCode :: Alex Int
alexGetStartCode = Alex $ \s@AlexState{alex_scd=sc} -> Right (s, sc)

alexSetStartCode :: Int -> Alex ()
alexSetStartCode sc = Alex $ \s -> Right (s{alex_scd=sc}, ())

alexMonadScan = do
  inp <- alexGetInput
  sc <- alexGetStartCode
  case alexScan inp sc of
    Nothing -> alexEOF inp 
    Just (inp', len, action) -> do
	alexSetInput inp'
	action inp len

-- -----------------------------------------------------------------------------
-- Useful token actions

-- just ignore this token and scan another one
skip input len = alexMonadScan

-- ignore this token, but set the start code to a new value
begin code input len = do alexSetStartCode code; alexMonadScan

-- perform an action for this token, and set the start code to a new value
(token `andBegin` code) input len = do alexSetStartCode code; token input len

#endif /* ALEX_MONAD */

-- -----------------------------------------------------------------------------
-- Basic wrapper

#ifdef ALEX_BASIC
-- alexScanTokens :: String -> [action]
alexScanTokens str = go (alexStartPos,'\n',str)
  where go inp@(_,_,str) =
	  case alexScan inp 0 of
		Nothing -> []
		Just (inp',len,act) -> act (take len str) : go inp'
#endif

-- -----------------------------------------------------------------------------
-- GScan wrapper

-- For compatibility with previous versions of Alex, and because we can.

#ifdef ALEX_GSCAN
alexGScan stop state inp = alex_gscan stop alexStartPos '\n' inp (0,state)

alex_gscan stop p c inp (IBOX(sc),state) =
  case alex_scan_tkn c ILIT(0) (p,c,inp) sc AlexNone of
	AlexNone -> stop p c inp (IBOX(sc),state)
	AlexLastAcc k (p',c',inp') len ->
 	     k p c inp len (\scs -> alex_gscan stop p' c' inp' scs) 
		(IBOX(sc),state)
#endif
