%START  NORM  VERB  MATH  SYNTAX
sp			[ \t]*
verb			\n{sp}@{sp}\n
math			\n{sp}\"{sp}\n
synt			\n{sp}@@@{sp}\n
nl			{sp}\n{sp}
%{
#define PUSH		states[top++] =
#define POP		BEGIN states[--top]
%}
%%
			int states[256];
			int top;
			BEGIN NORM;
			top = 0;
<NORM>@@		{ printf ("@"); }
<NORM>@			{ printf ("\\mbox{\\tt ");
			  PUSH NORM;  BEGIN VERB; }
<VERB>@			{ printf ("}");  POP; }
<NORM>{verb}		{ printf ("\n\\mbox{\\tt ");
			  PUSH NORM;  BEGIN VERB; }
<VERB>{verb}		{ printf ("}\n");  POP; }
<VERB>^{sp}\n		{ printf ("}\\\\[-4pt]\n\\mbox{\\tt "); }
<VERB>\n		{ printf ("}\\\\\n\\mbox{\\tt "); }
<VERB>" "		{ printf ("\\ "); }
<VERB>@@		{ printf ("@"); }
<VERB>\#		{ printf ("{\\char'43}"); }
<VERB>\$		{ printf ("{\\char'44}"); }
<VERB>\%		{ printf ("{\\char'45}"); }
<VERB>\&		{ printf ("{\\char'46}"); }
<VERB>\~		{ printf ("{\\char'176}"); }
<VERB>\_		{ printf ("{\\char'137}"); }
<VERB>\^		{ printf ("{\\char'136}"); }
<VERB>\\		{ printf ("{\\char'134}"); }
<VERB>\{		{ printf ("{\\char'173}"); }
<VERB>\}		{ printf ("{\\char'175}"); }
<NORM>\"\"		{ printf ("\""); }
<NORM>\"{sp}		{ printf ("\\mbox{$\\it ");
			  PUSH NORM;  BEGIN MATH; }
<MATH>{sp}\"		{ printf ("$}"); POP; }
<NORM>{math}{sp}	{ printf ("\n\\[\n\\it ");
			  PUSH NORM;  BEGIN MATH; }
<MATH>{sp}{math}	{ printf ("\n\\]\n"); POP; }
<MATH>{nl}		{ printf ("\\\\\n\\it "); }
<MATH>{sp}&{sp}		{ printf ("&\\it "); }
<MATH>\\{nl}		{ }
<MATH>{sp}		{ printf ("\\ "); }
<MATH>"..."		{ printf ("\\ldots "); }
<MATH>">="		{ printf ("\\geq "); }
<MATH>"<="		{ printf ("\\leq "); }
<MATH>"->"		{ printf ("\\rightarrow "); }
<MATH>"<-"		{ printf ("\\leftarrow "); }
<MATH>@@		{ printf ("@"); }
<MATH>@			{ printf ("\\makebox{\\tt ");
			  PUSH MATH;  BEGIN VERB; }
<NORM>{synt}{sp}	{ printf ("\n\\begin{flushleft}");
			  printf ("\\it\\begin{tabbing}\n");
			  printf ("\\hspace{0.5in}\\=");
			  printf ("\\hspace{3.0in}\\=\\kill\n$\\it "); 
			  BEGIN SYNTAX; }
<SYNTAX>{sp}{synt}	{ printf ("$\n\\end{tabbing}\\end{flushleft}\n"); 
			  BEGIN NORM; }
<SYNTAX>{nl}		{ printf ("$\\\\ \n$\\it "); }
<SYNTAX>{sp}"->"{sp}	{ printf ("$\\>\\makebox[3.5em]{$\\rightarrow$}");
			  printf ("$\\it "); }
<SYNTAX>{nl}"|"{sp}	{ printf ("$\\\\ \n$\\it "); 
			  printf ("$\\>\\makebox[3.5em]{$|$}$\\it "); }
<SYNTAX>{sp}&{sp}	{ printf ("$\\>\\makebox[3em]{}$\\it "); }
<SYNTAX>\\{nl}		{ }
<SYNTAX>{sp}		{ printf ("\\ "); }
<SYNTAX>"..."		{ printf ("\\ldots "); }
<SYNTAX>">="		{ printf ("\\geq "); }
<SYNTAX>"<="		{ printf ("\\leq "); }
<SYNTAX>"->"		{ printf ("\\rightarrow "); }
<SYNTAX>"<-"		{ printf ("\\leftarrow "); }
<SYNTAX>@@		{ printf ("@"); }
<SYNTAX>@		{ printf ("\\makebox{\\tt ");
			  PUSH SYNTAX;  BEGIN VERB; }

