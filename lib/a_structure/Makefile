# ----------------------------------------------
# @doc Arboreus Data structures handler
# (C) Arboreus library (http://arboreus.systems)
# (C) Alexandr Kirilov (http://alexandr.kirilov.me)
#
# ----------------------------------------------

MAKEFLAGS += --silent
THIS_FILE := $(lastword $(MAKEFILE_LIST))
.DEFAULT_GOAL: all


# ----------------------------------------------
# Extentions

include ../../make/directories.conf.mk
include ../../make/conf.mk
include ../../make/actions.mk


# ----------------------------------------------
# Variables

APP_NAME = a_structure


# ----------------------------------------------
# Targets

all: install clean

install: backup clean ensure_dir_silent
	$(call target,a_structure.erl)
	$(call target,a_structure_gb.erl)
	$(call target,a_structure_l.erl)
	$(call target,a_structure_m.erl)
	$(call target,a_structure_pl.erl)
	$(call target,a_structure_r.erl)
	$(call target,a_structure_t.erl)
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

backup:
	rm -rf $(APP_DIR_BACKUP_EBIN)/*; \
    $(call ensure_dir_silent,$(APP_DIR_BACKUP_EBIN)) \
    cp -Rf $(APP_DIR_EBIN)/ $(APP_DIR_BACKUP_EBIN) || exit; \
    $(call return_done_for_module,"Backup procedures for $(APP_NAME) finished.")


# ----------------------------------------------
# Erlang modules

a_structure.erl:
	$(call build_erl,a_structure)

a_structure_gb.erl:
	$(call build_erl,a_structure_gb)

a_structure_l.erl:
	$(call build_erl,a_structure_l)

a_structure_m.erl:
	$(call build_erl,a_structure_m)

a_structure_pl.erl:
	$(call build_erl,a_structure_pl)

a_structure_r.erl:
	$(call build_erl,a_structure_r)

a_structure_t.erl:
	$(call build_erl,a_structure_t)