# -----------------------------------------------------------------------------
# $Id: 

# alex Makefile, Isaac Jones
# The make uninstall rule doesn't work, but neither does Happy's seem to work

TOP = ..
include $(TOP)/mk/boilerplate.mk

INSTALLING=1

ALEX    = alex

HS_PROG = alex.bin

Parser_HC_OPTS      = -Onot
Version_HC_OPTS	    = -DALEX_VERSION=$(ProjectVersion)

# This is not a good idea for a source distribution
# SRC_ALEX_OPTS += -g

ALEX_BOOT = /home/simonmar/builds/i386-unknown-linux/happy-SAVE/alex/src/alex-inplace

Scan.hs : Scan.x
	 $(ALEX_BOOT) Scan.x

#tmp
SRC_HC_OPTS += -O

cmp :: Scan.hs
	./alex-inplace Scan.x -o Scan.hs-CMP
	cmp Scan.hs Scan.hs-CMP

ifeq "$(INSTALLING)" "1"
ifeq "$(BIN_DIST)"   "1"
ALEXLIB=$$\"\"libdir/alex
ALEXBIN=$$\"\"libexecdir/$(HS_PROG)
else
ALEXLIB=$(libdir)/alex
ALEXBIN=$(libexecdir)/$(HS_PROG)
endif # BIN_DIST
else
ALEXLIB=$(FPTOOLS_TOP_ABS)/happy/alex/templates
ALEXBIN=$(FPTOOLS_TOP_ABS)/happy/alex/src/$(HS_PROG)
endif

INSTALLED_SCRIPT_PROG  = alex-$(ProjectVersion)
INPLACE_SCRIPT_PROG    = alex-inplace

ifeq "$(INSTALLING)" "1"
TOP_PWD 	:= $(prefix)
SCRIPT_PROG 	=  $(INSTALLED_SCRIPT_PROG)
LINK	 	=  alex
else
TOP_PWD 	:= $(FPTOOLS_TOP_ABS)
SCRIPT_PROG 	=  $(INPLACE_SCRIPT_PROG)
endif

SCRIPT_OBJS=alex.sh

INTERP=$(SHELL)

SCRIPT_SUBST_VARS = 	\
	ALEXLIB	\
	ALEXBIN

INSTALL_EXECS = $(HS_PROG)
INSTALL_PROGS = $(HS_PROG)

# Hack to avoid automatic cleaning machinery from cleaning Parser.hs
#
DERIVED_ALEX_SRCS = 

# don't recurse on 'make install'
#
ifeq "$(INSTALLING)" "1"
all clean distclean maintainer-clean ::
	$(MAKE) INSTALLING=0 BIN_DIST=0 $(MFLAGS) $@
endif

include $(TOP)/mk/target.mk
