TOP = ..
include $(TOP)/mk/boilerplate.mk

.PRECIOUS: .hs .o .bin

TESTS = simple.x

ALEX=../src/alex-inplace

%.n.hs : %.x
	$(ALEX) $(TEST_ALEX_OPTS) $< -o $@

%.g.hs : %.x
	$(ALEX) $(TEST_ALEX_OPTS) -g $< -o $@

CLEAN_FILES += *.n.hs *.g.hs *.info *.hi *.bin

ALL_TEST_HS = $(shell echo $(TESTS) | sed -e 's/\([^\. ]*\)\.\(l\)\{0,1\}x/\1.n.hs \1.g.hs/g')

ALL_TESTS = $(patsubst %.hs, %.run, $(ALL_TEST_HS))

HC_OPTS += -fglasgow-exts -package lang

%.run : %.bin
	./$<

%.bin : %.o
	$(HC) $(HC_OPTS) $($*_LD_OPTS) $< -o $@

all :: $(ALL_TESTS)

include $(TOP)/mk/target.mk
