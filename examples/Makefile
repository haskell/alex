ALEX=../dist/build/alex/alex
HC=ghc -Wall -fno-warn-unused-binds -fno-warn-missing-signatures -fno-warn-unused-matches -fno-warn-name-shadowing -fno-warn-unused-imports -fno-warn-tabs

HAPPY=happy
HAPPY_OPTS=-agc

ifeq "$(TARGETPLATFORM)" "i386-unknown-mingw32"
exeext=.exe
else
exeext=.bin
endif

PROGS = lit Tokens Tokens_gscan words words_posn words_monad tiny haskell tiger

ALEX_OPTS = --template=.. -g
# ALEX_OPTS = --template=..

%.alex.hs : %.x
	$(ALEX) $(ALEX_OPTS) $< -o $@

%.happy.hs : %.y
	$(HAPPY) $(HAPPY_OPTS) $< -o $@

%.o : %.hs
	$(HC) $(HC_OPTS) -c -o $@ $<

CLEAN_FILES += *.info *.hi *.o *.bin *.exe

all : $(addsuffix $(exeext),$(PROGS))

tiny$(exeext) : tiny.happy.hs Tokens_posn.alex.hs
	$(HC) $(HC_OPTS) -o $@ $^

lit$(exeext) : lit.alex.hs
	$(HC) $(HC_OPTS) -o $@ $^

Tokens$(exeext) : Tokens.alex.hs
	$(HC) $(HC_OPTS) -o $@ $^

Tokens_gscan$(exeext) : Tokens_gscan.alex.hs
	$(HC) $(HC_OPTS) -o $@ $^

words$(exeext) : words.alex.hs
	$(HC) $(HC_OPTS) -o $@ $^

words_posn$(exeext) : words_posn.alex.hs
	$(HC) $(HC_OPTS) -o $@ $^

words_monad$(exeext) : words_monad.alex.hs
	$(HC) $(HC_OPTS) -o $@ $^

haskell$(exeext) : haskell.alex.hs
	$(HC) $(HC_OPTS) -o $@ $^

tiger$(exeext) : tiger.alex.hs
	$(HC) $(HC_OPTS) -main-is TigerLexer -o $@ $^

.PHONY: clean
clean:
	rm -f *.o *.hi $(addsuffix $(exeext),$(PROGS)) \
		*.alex.hs *.happy.hs
