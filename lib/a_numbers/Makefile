# ----------------------------------------------
# @doc Arboreus Numbers data structures handler
# (C) Arboreus library (http://arboreus.systems)
# (C) Alexandr Kirilov (http://alexandr.kirilov.me)
#
# ----------------------------------------------

MAKEFLAGS += --silent
THIS_FILE := $(lastword $(MAKEFILE_LIST))
.DEFAULT_GOAL: all


# ----------------------------------------------
# Extentions

include ../../make/conf.mk
include ../../make/actions.mk


# ----------------------------------------------
# Variables

APP_NAME = a_numbers


# ----------------------------------------------
# Targets

all: install clean

install:
	$(call return_done_for_module,"Module $(APP_NAME) installed.")

clean:
	$(call return_done_for_module,"Cleaning for module $(APP_NAME) finished.")