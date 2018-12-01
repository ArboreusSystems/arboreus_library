# ----------------------------------------------
# @doc Arboreus Tests handler
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

APP_NAME = a_test


# ----------------------------------------------
# Targets

all: install clean

install: clean ensure_dir_silent link_objects
	$(call target,a_test.erl)
	$(call return_done_for_module,"Module $(APP_NAME) installed.")

clean:
	rm -rf $(DIR_BUILD)/$(APP_NAME)/*.o; \
    $(call return_done_for_module,"Cleaning for module $(APP_NAME) finished.")

configure: ensure_dir
	$(call return_done_for_module,"Configuring procedures for $(APP_NAME) finished")

ensure_dir:
	$(call ensure_dir,$(APP_DIR_EBIN))
	$(call ensure_dir,$(APP_DIR_BIN))
	$(call ensure_dir,$(APP_DIR_BUILD))

ensure_dir_silent:
	$(call ensure_dir,$(APP_DIR_EBIN))
	$(call ensure_dir_silent,$(APP_DIR_BIN))
	$(call ensure_dir_silent,$(APP_DIR_BUILD))

link_objects: build_objects
	$(COMPILLER_C) -o $(DIR_BIN)/$(APP_NAME)/a_test \
	 $(call dir_module_build,$(APP_NAME))/a_test_main.o \
	 || exit;

build_objects: a_test_main.c
	$(call return_done_for_module,"Objects built for module $(APP_NAME).")


# ----------------------------------------------
# C modules

a_test_main.c:
	$(call build_o,a_test_main,$(APP_NAME))


# ----------------------------------------------
# Erlang modules

a_test.erl:
	$(call build_erl,$(APP_DIR_SRC)/a_test.erl,$(APP_DIR_EBIN))