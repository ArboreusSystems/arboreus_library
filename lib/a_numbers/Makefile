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

install: ensure_dir_silent
	$(call target,a_numbers.erl)
	$(call target,a_numbers_gb.erl)
	$(call target,a_numbers_l.erl)
	$(call target,a_numbers_m.erl)
	$(call target,a_numbers_pl.erl)
	$(call target,a_numbers_r.erl)
	$(call target,a_numbers_t.erl)
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

a_numbers.erl:
	$(call build_erl,$(APP_DIR_SRC)/a_numbers.erl,$(APP_DIR_EBIN))

a_numbers_gb.erl:
	$(call build_erl,$(APP_DIR_SRC)/a_numbers_gb.erl,$(APP_DIR_EBIN))

a_numbers_l.erl:
	$(call build_erl,$(APP_DIR_SRC)/a_numbers_l.erl,$(APP_DIR_EBIN))

a_numbers_m.erl:
	$(call build_erl,$(APP_DIR_SRC)/a_numbers_m.erl,$(APP_DIR_EBIN))

a_numbers_pl.erl:
	$(call build_erl,$(APP_DIR_SRC)/a_numbers_pl.erl,$(APP_DIR_EBIN))

a_numbers_r.erl:
	$(call build_erl,$(APP_DIR_SRC)/a_numbers_r.erl,$(APP_DIR_EBIN))

a_numbers_t.erl:
	$(call build_erl,$(APP_DIR_SRC)/a_numbers_t.erl,$(APP_DIR_EBIN))