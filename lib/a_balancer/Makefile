# ----------------------------------------------
# @doc Arboreus Balancer
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

APP_NAME = a_balancer


# ----------------------------------------------
# Targets

all: install clean

install: ensure_dir_silent
	$(call target,a_balancer.erl)
	$(call target,a_balancer_supervisor.erl)
	$(call target,abal_client.erl)
	$(call target,abal_server.erl)
	$(call target,a_balancer.app)
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

a_balancer.erl:
	$(call build_erl,$(APP_DIR_SRC)/a_balancer.erl,$(APP_DIR_EBIN))

a_balancer_supervisor.erl:
	$(call build_erl,$(APP_DIR_SRC)/a_balancer_supervisor.erl,$(APP_DIR_EBIN))

abal_client.erl:
	$(call build_erl,$(APP_DIR_SRC)/abal_client.erl,$(APP_DIR_EBIN))

abal_server.erl:
	$(call build_erl,$(APP_DIR_SRC)/abal_server.erl,$(APP_DIR_EBIN))

a_balancer.app:
	$(call build_app,$(APP_DIR_SRC)/a_balancer.app,$(APP_DIR_EBIN))