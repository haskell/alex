TOP = .
include $(TOP)/mk/boilerplate.mk

ifneq "$(SGMLDocWays)" ""
DOCS_DIR = doc
else
DOCS_DIR =
endif

SUBDIRS = src templates $(DOCS_DIR)

include $(TOP)/mk/target.mk
