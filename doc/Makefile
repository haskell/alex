# Thanks to the creator of the excellent verbatim utility supplied with the
# Haskell-1.2 report.  It has eased the writing of alex documentation
# considerably.

alex.dvi: alex.tex
	latex alex.tex
	latex alex.tex

alex.tex: verbatim alex.verb
	expand < alex.verb | ./verbatim >alex.tex

verbatim: verbatim.lex
	lex verbatim.lex
	$(CC) lex.yy.c -ll -o verbatim
	$(RM) lex.yy.c

clean:
	rm -f verbatim alex.tex *.aux *.dvi *.log
