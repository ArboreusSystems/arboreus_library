# ----------------------------------------------
# @doc Arboreus User data and authorisation handler
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

APP_NAME = a_users


# ----------------------------------------------
# Targets

all: install clean

install: backup clean ensure_dir_silent
	$(call target,a_user.erl)
	$(call target,a_user_properties.erl)
	$(call target,a_users.erl)
	$(call target,a_users_supervisor.erl)
	$(call target,ause_auth.erl)
	$(call target,ause_login.erl)
	$(call target,ause_login_kind.erl)
	$(call target,ause_mdb.erl)
	$(call target,ause_properties.erl)
	$(call target,ause_user.erl)
	$(call target,a_users.app)
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

a_user.erl:
	$(call build_erl,$(APP_DIR_SRC)/a_user.erl,$(APP_DIR_EBIN))

a_user_properties.erl:
	$(call build_erl,$(APP_DIR_SRC)/a_user_properties.erl,$(APP_DIR_EBIN))

a_users.erl:
	$(call build_erl,$(APP_DIR_SRC)/a_users.erl,$(APP_DIR_EBIN))

a_users_supervisor.erl:
	$(call build_erl,$(APP_DIR_SRC)/a_users_supervisor.erl,$(APP_DIR_EBIN))

ause_auth.erl:
	$(call build_erl,$(APP_DIR_SRC)/ause_auth.erl,$(APP_DIR_EBIN))

ause_login.erl:
	$(call build_erl,$(APP_DIR_SRC)/ause_login.erl,$(APP_DIR_EBIN))

ause_login_kind.erl:
	$(call build_erl,$(APP_DIR_SRC)/ause_login_kind.erl,$(APP_DIR_EBIN))

ause_mdb.erl:
	$(call build_erl,$(APP_DIR_SRC)/ause_mdb.erl,$(APP_DIR_EBIN))

ause_properties.erl:
	$(call build_erl,$(APP_DIR_SRC)/ause_properties.erl,$(APP_DIR_EBIN))

ause_user.erl:
	$(call build_erl,$(APP_DIR_SRC)/ause_user.erl,$(APP_DIR_EBIN))

a_users.app:
	$(call build_app,$(APP_DIR_SRC)/a_users.app,$(APP_DIR_EBIN))