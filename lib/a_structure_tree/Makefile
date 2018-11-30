# ----------------------------------------------
# @doc Arboreus Tree-Like structures handler
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

APP_NAME = a_structure_tree


# ----------------------------------------------
# Targets

all: install clean

install: clean ensure_dir_silent
	$(call target,astr_alias.erl)
	$(call target,astr_link.erl)
	$(call target,astr_mdb.erl)
	$(call target,astr_point.erl)
	$(call target,astr_test.erl)
	$(call return_done_for_module,"Module $(APP_NAME) installed.")

clean:
	rm -rf $(DIR_EBIN)/$(APP_NAME)/*; \
	$(call return_done_for_module,"Cleaning for module $(APP_NAME) finished.")

configure: ensure_dir
	$(call return_done_for_module,"Configuring procedures for $(APP_NAME) finished")

ensure_dir:
	$(call ensure_dir,$(APP_DIR_EBIN))

ensure_dir_silent:
	$(call ensure_dir_silent,$(APP_DIR_EBIN))


# ----------------------------------------------
# Erlang modules

astr_alias.erl:
	$(call build_erl,$(APP_DIR_SRC)/astr_alias.erl,$(APP_DIR_EBIN))

astr_link.erl:
	$(call build_erl,$(APP_DIR_SRC)/astr_link.erl,$(APP_DIR_EBIN))

astr_mdb.erl:
	$(call build_erl,$(APP_DIR_SRC)/astr_mdb.erl,$(APP_DIR_EBIN))

astr_point.erl:
	$(call build_erl,$(APP_DIR_SRC)/astr_point.erl,$(APP_DIR_EBIN))

astr_test.erl:
	$(call build_erl,$(APP_DIR_SRC)/astr_test.erl,$(APP_DIR_EBIN))
