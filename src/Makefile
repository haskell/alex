TOP = ..
include $(TOP)/mk/boilerplate.mk

INSTALLING=1

ifeq "$(TARGETPLATFORM)" "i386-unknown-mingw32"
HS_PROG_EXT = .exe
else
HS_PROG_EXT = .bin
endif

HS_PROG = alex$(HS_PROG_EXT)

Main_HC_OPTS =  -DALEX_VERSION=$(ProjectVersion) 

ifeq "$(Windows)" "YES"
Main_HC_OPTS += -Dmingw32_HOST_OS=1
endif

# -----------------------------------------------------------------------------=
# Create the Version.hs file

VERSION_HS = Version.hs
EXTRA_SRCS += $(VERSION_HS)

boot :: $(VERSION_HS)

Version.hs : ../mk/version.mk
	@$(RM) -f $(VERSION_HS)
	@echo "Creating $(VERSION_HS) ... "
	@echo "module Version where" >>$(VERSION_HS)
	@echo "version = \"$(ProjectVersion)\"" >> $(VERSION_HS)

# -----------------------------------------------------------------------------

ifeq "$(INSTALLING)" "1"
ifeq "$(BIN_DIST)"   "1"
ALEXLIB=$$\"\"libdir
ALEXBIN=$$\"\"libexecdir/$(HS_PROG)
else
ALEXLIB=$(libdir)
ALEXBIN=$(libexecdir)/$(HS_PROG)
endif # BIN_DIST
else
ALEXLIB=$(FPTOOLS_TOP_ABS)/alex/templates
ALEXBIN=$(FPTOOLS_TOP_ABS)/alex/src/$(HS_PROG)
endif

INSTALLED_SCRIPT_PROG  = alex-$(ProjectVersion)
INPLACE_SCRIPT_PROG    = alex-inplace

ifeq "$(INSTALLING)" "1"
TOP_PWD 	:= $(prefix)
SCRIPT_PROG 	=  $(INSTALLED_SCRIPT_PROG)
ifneq "$(TARGETPLATFORM)" "i386-unknown-mingw32"
LINK	 	=  alex
endif
else
TOP_PWD 	:= $(FPTOOLS_TOP_ABS)
SCRIPT_PROG 	=  $(INPLACE_SCRIPT_PROG)
endif

SCRIPT_OBJS=alex.sh

INTERP=$(SHELL)

SCRIPT_SUBST_VARS = 	\
	ALEXLIB	\
	ALEXBIN

# The script isn't installed on Windows; we run the binary directly
ifeq "$(Windows)" "NO"
INSTALL_SCRIPTS += $(SCRIPT_PROG)
endif

INSTALL_LIBEXECS = $(HS_PROG)

# Hack to avoid automatic cleaning machinery from cleaning Scan.hs
#
DERIVED_ALEX_SRCS = 

# don't recurse on 'make install'
#
ifeq "$(INSTALLING)" "1"
all clean distclean maintainer-clean ::
	$(MAKE) INSTALLING=0 BIN_DIST=0 $(MFLAGS) $@
endif

# -----------------------------------------------------------------------------
# Testing

cmp :: Scan.hs
	./alex-inplace Scan.x -o Scan.hs-CMP
	cmp Scan.hs Scan.hs-CMP

CLEAN_FILES += Scan.hs-CMP

# -----------------------------------------------------------------------------

include $(TOP)/mk/target.mk
