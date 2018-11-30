# ----------------------------------------------
# @doc Arboreus Event Storage
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

APP_NAME = a_event_storage


# ----------------------------------------------
# Targets

all: install clean

install: ensure_dir_silent
	$(call target,aest_btree.erl)
	$(call target,aest_handler.erl)
	$(call return_done_for_module,"Module $(APP_NAME) installed.")

clean:
	$(call return_done_for_module,"Nothing for clean in module $(APP_NAME).")

configure: ensure_dir
	$(call return_done_for_module,"Configuring procedures for $(APP_NAME) finished")

ensure_dir:
	$(call ensure_dir,$(APP_DIR_EBIN))

ensure_dir_silent:
	$(call ensure_dir_silent,$(APP_DIR_EBIN))


# ----------------------------------------------
# Erlang modules

aest_btree.erl:
	$(call build_erl,$(APP_DIR_SRC)/aest_btree.erl,$(APP_DIR_EBIN))

aest_handler.erl:
	$(call build_erl,$(APP_DIR_SRC)/aest_handler.erl,$(APP_DIR_EBIN))