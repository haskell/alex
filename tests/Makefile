TOP = ..
include $(TOP)/mk/boilerplate.mk

.PRECIOUS: .hs .o .exe .bin

ifeq "$(TARGETPLATFORM)" "i386-unknown-mingw32"
HS_PROG_EXT = .exe
else
HS_PROG_EXT = .bin
endif


TESTS = simple.x tokens.x tokens_posn.x tokens_gscan.x

ALEX=../src/alex-inplace

%.n.hs : %.x
	$(ALEX) $(TEST_ALEX_OPTS) $< -o $@

%.g.hs : %.x
	$(ALEX) $(TEST_ALEX_OPTS) -g $< -o $@

CLEAN_FILES += *.n.hs *.g.hs *.info *.hi *.bin *.exe

ALL_TEST_HS = $(shell echo $(TESTS) | sed -e 's/\([^\. ]*\)\.\(l\)\{0,1\}x/\1.n.hs \1.g.hs/g')

ALL_TESTS = $(patsubst %.hs, %.run, $(ALL_TEST_HS))

HC_OPTS += -fglasgow-exts -package lang

%.run : %$(HS_PROG_EXT)
	./$<

%$(HS_PROG_EXT) : %.o
	$(HC) $(HC_OPTS) $($*_LD_OPTS) $< -o $@

all :: $(ALL_TESTS)

include $(TOP)/mk/target.mk
