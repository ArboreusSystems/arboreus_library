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

include ../../make/directories.conf.mk
include ../../make/conf.mk
include ../../make/actions.mk


# ----------------------------------------------
# Variables

APP_NAME = a_event_storage


# ----------------------------------------------
# Targets

all: install clean

install: backup clean ensure_dir_silent
	$(call target,aest_btree.erl)
	$(call target,aest_handler.erl)
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

aest_btree.erl:
	$(call build_erl,aest_btree)

aest_handler.erl:
	$(call build_erl,aest_handler)