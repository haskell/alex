ALEX=../dist/build/alex/alex
HC=ghc -Wall -fno-warn-unused-binds -fno-warn-missing-signatures -fno-warn-unused-matches -fno-warn-name-shadowing -fno-warn-unused-imports

HAPPY=happy
HAPPY_OPTS=-agc

ifeq "$(TARGETPLATFORM)" "i386-unknown-mingw32"
exeext=.exe
else
exeext=
endif

PROGS = lit Tokens Tokens_gscan words words_posn words_monad tiny haskell

ALEX_OPTS = --template=.. -g
# ALEX_OPTS = --template=..

%.hs : %.x
	$(ALEX) $(ALEX_OPTS) $< -o $@

%.hs : %.y
	$(HAPPY) $(HAPPY_OPTS) $< -o $@

%.o : %.hs
	$(HC) $(HC_OPTS) -c -o $@ $<

CLEAN_FILES += *.info *.hi *.o *.bin *.exe

all : $(addsuffix $(HS_PROG_EXT),$(PROGS))

tiny$(exeext) : tiny.hs Tokens_posn.hs
	$(HC) $(HC_OPTS) -o $@ $^

lit$(exeext) : lit.hs
	$(HC) $(HC_OPTS) -o $@ $^

Tokens$(exeext) : Tokens.hs
	$(HC) $(HC_OPTS) -o $@ $^

Tokens_gscan$(exeext) : Tokens_gscan.hs
	$(HC) $(HC_OPTS) -o $@ $^

words$(exeext) : words.hs
	$(HC) $(HC_OPTS) -o $@ $^

words_posn$(exeext) : words_posn.hs
	$(HC) $(HC_OPTS) -o $@ $^

words_monad$(exeext) : words_monad.hs
	$(HC) $(HC_OPTS) -o $@ $^

haskell$(exeext) : haskell.hs
	$(HC) $(HC_OPTS) -o $@ $^

.PHONY: clean
clean:
	rm -f *.o *.hi *.bin $(addsuffix $(exeext),$(PROGS)) \
		$(addsuffix .hs,$(PROGS)) Tokens_posn.hs

