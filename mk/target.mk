# We need to set TOP to be the TOP that the next level up expects!
# The TOP variable is reset after the inclusion of the fptools
# boilerplate, so we stash TOP away first:
ALEX_TOP := $(TOP)
TOP:=$(TOP)/..

include $(TOP)/mk/target.mk

# Reset TOP
TOP:=$(ALEX_TOP)
