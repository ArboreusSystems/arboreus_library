# ----------------------------------------------
# @doc Arboreus addons for Yaws
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

APP_NAME = yaws_addons


# ----------------------------------------------
# Targets

all: install clean

install: backup clean ensure_dir_silent
	$(call target,a_http_headers.erl)
	$(call target,a_output.erl)
	$(call target,a_output_dsv.erl)
	$(call target,a_output_json.erl)
	$(call target,a_output_xml.erl)
	$(call target,a_params.erl)
	$(call target,a_stub.erl)
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
    cp -Rf $(APP_DIR_EBIN)/* $(APP_DIR_BACKUP_EBIN) || exit; \
    $(call return_done_for_module,"Backup procedures for $(APP_NAME) finished.")


# ----------------------------------------------
# Erlang modules

a_http_headers.erl:
	$(call build_erl,a_http_headers)

a_output.erl:
	$(call build_erl,a_output)

a_output_dsv.erl:
	$(call build_erl,a_output_dsv)

a_output_json.erl:
	$(call build_erl,a_output_json)

a_output_xml.erl:
	$(call build_erl,a_output_xml)

a_params.erl:
	$(call build_erl,a_params)

a_stub.erl:
	$(call build_erl,a_stub)
