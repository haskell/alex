TOP = ..
include $(TOP)/mk/boilerplate.mk

TEMPLATES = \
	AlexTemplate \
	AlexTemplate-ghc \
	AlexTemplate-ghc-debug \
	AlexTemplate-debug \
	AlexWrapper-basic \
	AlexWrapper-posn \
	AlexWrapper-monad \
	AlexWrapper-gscan

GENERIC_TEMPLATE = GenericTemplate.hs
EXCLUDED_SRCS = $(GENERIC_TEMPLATE) wrappers.hs

INSTALL_DATAS = $(TEMPLATES)

all :: $(TEMPLATES)

override datadir = $(libdir)

ghc_411_at_least = $(shell expr "$(GhcMajVersion)"  =   4 \& \
                                "$(GhcMinVersion)" \>= 11 \| \
                                "$(GhcMajVersion)" \>   4)

ifeq "$(ghc_411_at_least)" "1"
CPP_IT = $(GHC) -E -cpp -o
else
CPP_IT = $(GHC) -E -cpp >
endif

AlexTemplate.hspp : $(GENERIC_TEMPLATE)
	$(CPP_IT) $@ $(GENERIC_TEMPLATE)

AlexTemplate-ghc.hspp : $(GENERIC_TEMPLATE)
	$(CPP_IT) $@ -DALEX_GHC $(GENERIC_TEMPLATE)

AlexTemplate-ghc-debug.hspp : $(GENERIC_TEMPLATE)
	$(CPP_IT) $@ -DALEX_GHC -DALEX_DEBUG $(GENERIC_TEMPLATE)

AlexTemplate-debug.hspp : $(GENERIC_TEMPLATE)
	$(CPP_IT) $@ -DALEX_DEBUG $(GENERIC_TEMPLATE)

AlexTemplate-monad.hspp : $(GENERIC_TEMPLATE)
	$(CPP_IT) $@ -DALEX_MONAD $(GENERIC_TEMPLATE)

AlexWrapper-basic.hspp : wrappers.hs
	$(CPP_IT) $@ -DALEX_BASIC $<

AlexWrapper-posn.hspp : wrappers.hs
	$(CPP_IT) $@ -DALEX_POSN $<

AlexWrapper-monad.hspp : wrappers.hs
	$(CPP_IT) $@ -DALEX_MONAD $<

AlexWrapper-gscan.hspp : wrappers.hs
	$(CPP_IT) $@ -DALEX_GSCAN $<

# hack to turn cpp-style '# 27 "GenericTemplate.hs"' into 
# '{-# LINE 27 "GenericTemplate.hs" #-}'.
% : %.hspp
	perl -pe 's/^#\s+(\d+)\s+(\"[^\"]*\")/{-# LINE \1 \2 #-}/g;s/\$$(Id:.*)\$$/\1/g' < $< > $@

CLEAN_FILES += $(TEMPLATES) $(patsubst %, %.hspp, $(TEMPLATES))

include $(TOP)/mk/target.mk
