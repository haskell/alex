{
import Alex
import Tokens
}

%name calc
%tokentype { Token }

%token  let		{ Let _     }
	in		{ In  _     }
	int		{ Int _ $$  }
	var		{ Var _ $$  }
	'='		{ Sym _ '=' }
	'+'		{ Sym _ '+' }
	'-'		{ Sym _ '-' }
	'*'		{ Sym _ '*' }
	'/'		{ Sym _ '/' }
	'('		{ Sym _ '(' }
	')'		{ Sym _ ')' }
	error		{ Err _     }

%%

Exp :: { Exp }
Exp : let var '=' Exp in Exp	{ LetE $2 $4 $6 }
    | Exp1			{ $1            }

Exp1 : Exp1 '+' Term		{ PlusE  $1 $3  }
     | Exp1 '-' Term		{ MinusE $1 $3  }
     | Term			{ $1            }

Term : Term '*' Factor		{ TimesE $1 $3  }
     | Term '/' Factor		{ DivE $1 $3    }
     | Factor			{ $1            }

Factor : '-' Atom		{ NegE $2	}
       | Atom			{ $1		}

Atom : int			{ IntE $1       }
       | var			{ VarE $1       }
       | '(' Exp ')'		{ $2            }

{
data Exp =
	LetE   String Exp Exp |
	PlusE  Exp Exp        |
	MinusE Exp Exp        |
	TimesE Exp Exp        |
	DivE   Exp Exp        |
	NegE   Exp	      |
	IntE   Int            |
	VarE   String
	deriving Show


main:: IO ()
main = interact (show.runCalc)

runCalc :: String -> Exp
runCalc = calc . tokens

happyError :: Int -> [Token] -> a
happyError i tks = error ("Parse error at " ++ lcn ++ "\n")
	where
	lcn = 	case tks of
		  [] -> "end of file"
		  tk:_ -> "line " ++ show l ++ ", column " ++ show c
			where
			Pn _ l c = token_pos tk
}
