TOP = ..
include $(TOP)/mk/boilerplate.mk

PROGS = lit Tokens Tokens_posn Tokens_gscan words words_posn words_monad \
	tiny haskell

lit : lit.hs
	$(HC) $(HC_OPTS) -o $@ $<

tiny : Tokens_posn.o tiny.o
	$(HC) $(HC_OPTS) -o $@ Tokens_posn.o tiny.o

Tokens_posn : Tokens_posn.hs
	$(HC) $(HC_OPTS) -o $@ $<

Tokens_gscan : Tokens_gscan.hs
	$(HC) $(HC_OPTS) -o $@ $<

words : words.hs
	$(HC) $(HC_OPTS) -o $@ $<

words_posn : words_posn.hs
	$(HC) $(HC_OPTS) -o $@ $<

words_monad : words_monad.hs
	$(HC) $(HC_OPTS) -o $@ $<

haskell : haskell.hs
	$(HC) $(HC_OPTS) -o $@ $<

include $(TOP)/mk/target.mk
