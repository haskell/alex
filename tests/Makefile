ALEX=../dist/build/alex/alex
HC=ghc
HC_OPTS=-Wall -fno-warn-unused-binds -fno-warn-missing-signatures -fno-warn-unused-matches -fno-warn-name-shadowing -fno-warn-unused-imports

.PRECIOUS: %.n.hs %.g.hs %.o %.exe %.bin

ifeq "$(TARGETPLATFORM)" "i386-unknown-mingw32"
HS_PROG_EXT = .exe
else
HS_PROG_EXT = .bin
endif

TESTS = unicode.x simple.x tokens.x tokens_posn.x tokens_gscan.x tokens_bytestring.x tokens_posn_bytestring.x tokens_strict_bytestring.x

TEST_ALEX_OPTS = --template=..

%.n.hs : %.x
	$(ALEX) $(TEST_ALEX_OPTS) $< -o $@

%.g.hs : %.x
	$(ALEX) $(TEST_ALEX_OPTS) -g $< -o $@

%.o : %.hs
	$(HC) $(HC_OPTS) -c -o $@ $<

CLEAN_FILES += *.n.hs *.g.hs *.info *.hi *.o *.bin *.exe

ALL_TEST_HS = $(shell echo $(TESTS) | sed -e 's/\([^\. ]*\)\.\(l\)\{0,1\}x/\1.n.hs \1.g.hs/g')

ALL_TESTS = $(patsubst %.hs, %.run, $(ALL_TEST_HS))

%.run : %$(HS_PROG_EXT)
	./$<

%$(HS_PROG_EXT) : %.o
	$(HC) $(HC_OPTS) -package array -package bytestring $($*_LD_OPTS) $< -o $@

all :: $(ALL_TESTS)

.PHONY: clean
clean:
	rm -f $(CLEAN_FILES)

interact:
	ghci -cpp -i../src -i../dist/build/autogen -i../dist/build/alex/alex-tmp Main -fbreak-on-exception 
# -args='--template=.. simple.x -o simple.n.hs'
# :set args --template=.. simple.x -o simple.n.hs
