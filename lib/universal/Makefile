# ----------------------------------------------
# @doc Arboreus Library universal modules
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

APP_NAME = universal


# ----------------------------------------------
# Targets

all: install clean

install: backup clean ensure_dir_silent
	$(call target,a_code.erl)
	$(call target,a_config.erl)
	$(call target,a_file.erl)
	$(call target,a_list.erl)
	$(call target,a_lpath.erl)
	$(call target,a_matrix.erl)
	$(call target,a_mnesia.erl)
	$(call target,a_net.erl)
	$(call target,a_node.erl)
	$(call target,a_proplists.erl)
	$(call target,a_qlc.erl)
	$(call target,a_rpc.erl)
	$(call target,a_sequence.erl)
	$(call target,a_term.erl)
	$(call target,a_text.erl)
	$(call target,a_time.erl)
	$(call target,a_value_bin_is.erl)
	$(call target,a_value_str_is.erl)
	$(call target,a_var.erl)
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

a_code.erl:
	$(call build_erl,$(APP_DIR_SRC)/a_code.erl,$(APP_DIR_EBIN))

a_config.erl:
	$(call build_erl,$(APP_DIR_SRC)/a_config.erl,$(APP_DIR_EBIN))

a_file.erl:
	$(call build_erl,$(APP_DIR_SRC)/a_file.erl,$(APP_DIR_EBIN))

a_list.erl:
	$(call build_erl,$(APP_DIR_SRC)/a_list.erl,$(APP_DIR_EBIN))

a_lpath.erl:
	$(call build_erl,$(APP_DIR_SRC)/a_lpath.erl,$(APP_DIR_EBIN))

a_matrix.erl:
	$(call build_erl,$(APP_DIR_SRC)/a_matrix.erl,$(APP_DIR_EBIN))

a_mnesia.erl:
	$(call build_erl,$(APP_DIR_SRC)/a_mnesia.erl,$(APP_DIR_EBIN))

a_net.erl:
	$(call build_erl,$(APP_DIR_SRC)/a_net.erl,$(APP_DIR_EBIN))

a_node.erl:
	$(call build_erl,$(APP_DIR_SRC)/a_node.erl,$(APP_DIR_EBIN))

a_proplists.erl:
	$(call build_erl,$(APP_DIR_SRC)/a_proplists.erl,$(APP_DIR_EBIN))

a_qlc.erl:
	$(call build_erl,$(APP_DIR_SRC)/a_qlc.erl,$(APP_DIR_EBIN))

a_rpc.erl:
	$(call build_erl,$(APP_DIR_SRC)/a_rpc.erl,$(APP_DIR_EBIN))

a_sequence.erl:
	$(call build_erl,$(APP_DIR_SRC)/a_sequence.erl,$(APP_DIR_EBIN))

a_term.erl:
	$(call build_erl,$(APP_DIR_SRC)/a_term.erl,$(APP_DIR_EBIN))

a_text.erl:
	$(call build_erl,$(APP_DIR_SRC)/a_text.erl,$(APP_DIR_EBIN))

a_time.erl:
	$(call build_erl,$(APP_DIR_SRC)/a_time.erl,$(APP_DIR_EBIN))

a_value_bin_is.erl:
	$(call build_erl,$(APP_DIR_SRC)/a_value_bin_is.erl,$(APP_DIR_EBIN))

a_value_str_is.erl:
	$(call build_erl,$(APP_DIR_SRC)/a_value_str_is.erl,$(APP_DIR_EBIN))

a_var.erl:
	$(call build_erl,$(APP_DIR_SRC)/a_var.erl,$(APP_DIR_EBIN))