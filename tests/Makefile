# NOTE: `cabal test` will take care to build the local `alex`
# executable and place it into $PATH for us to pick up.  
# 
# If it doesn't look like the alex binary in $PATH comes from the
# build tree, then we'll fall back to pointing to
# ../dist/build/alex/alex to support running tests via "runghc
# Setup.hs test".
#
ALEX=$(shell which alex)
ifeq "$(filter $(dir $(shell pwd))%,$(ALEX))" ""
ALEX=../dist/build/alex/alex
endif

# NOTE: This assumes that a working `ghc` is on $PATH; this may not necessarily be the same GHC used by `cabal` for building `alex`.
HC=ghc
HC_OPTS=-Wall -fwarn-incomplete-uni-patterns -fno-warn-missing-signatures -fno-warn-unused-imports -fno-warn-tabs -Werror

.PRECIOUS: %.n.hs %.g.hs %.o %.exe %.bin

ifeq "$(TARGETPLATFORM)" "i386-unknown-mingw32"
HS_PROG_EXT = .exe
else
HS_PROG_EXT = .bin
endif

TESTS = \
        basic_typeclass.x \
        basic_typeclass_bytestring.x \
        default_typeclass.x \
        gscan_typeclass.x \
        monad_typeclass.x \
        monad_typeclass_bytestring.x \
        monadUserState_typeclass.x \
        monadUserState_typeclass_bytestring.x \
        null.x \
        posn_typeclass.x \
        posn_typeclass_bytestring.x \
        strict_typeclass.x \
        simple.x \
        tokens.x \
        tokens_bytestring.x \
        tokens_bytestring_unicode.x \
        tokens_gscan.x \
        tokens_monad_bytestring.x \
        tokens_monadUserState_bytestring.x \
        tokens_posn.x \
        tokens_posn_bytestring.x \
        tokens_scan_user.x \
        tokens_strict_bytestring.x \
        unicode.x

# NOTE: `cabal` will set the `alex_datadir` env-var accordingly before invoking the test-suite
#TEST_ALEX_OPTS = --template=../data/
TEST_ALEX_OPTS=

%.n.hs : %.x
	$(ALEX) $(TEST_ALEX_OPTS) $< -o $@

%.g.hs : %.x
	$(ALEX) $(TEST_ALEX_OPTS) -g $< -o $@

CLEAN_FILES += *.n.hs *.g.hs *.info *.hi *.o *.bin *.exe

ALL_TEST_HS = $(shell echo $(TESTS) | sed -e 's/\([^\. ]*\)\.\(l\)\{0,1\}x/\1.n.hs \1.g.hs/g')

ALL_TESTS = $(patsubst %.hs, %.run, $(ALL_TEST_HS))

%.run : %$(HS_PROG_EXT)
	./$<

%$(HS_PROG_EXT) : %.hs
	$(HC) $(HC_OPTS) -package array -package bytestring $($*_LD_OPTS) $< -o $@

all :: $(ALL_TESTS)

.PHONY: clean
clean:
	rm -f $(CLEAN_FILES)

# NOTE: The `../dist` path belows don't aren't accurate anymore for recent cabals
interact:
	ghci -cpp -i../src -i../dist/build/autogen -i../dist/build/alex/alex-tmp Main -fbreak-on-exception
# -args='--template=.. simple.x -o simple.n.hs'
# :set args --template=.. simple.x -o simple.n.hs
