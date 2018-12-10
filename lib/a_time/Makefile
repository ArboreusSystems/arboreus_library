# ----------------------------------------------
# @doc Arboreus time handler
# (C) Arboreus library (http://arboreus.systems)
# (C) Alexandr Kirilov (http://alexandr.kirilov.me)
#
# ----------------------------------------------

MAKEFLAGS += --silent
THIS_FILE := $(lastword $(MAKEFILE_LIST))
.DEFAULT_GOAL: all


# ----------------------------------------------
# Extentions

include ../../make/erlang.conf.mk
include ../../make/conf.mk
include ../../make/actions.mk


# ----------------------------------------------
# Variables

APP_NAME = a_time


# ----------------------------------------------
# Targets

all: install clean

install: backup clean ensure_dir_silent link_objects build_nifs
	$(call target,a_time.erl)
	$(call return_done_for_module,"Module $(APP_NAME) installed.")

clean:
	rm -rf $(DIR_BUILD)/$(APP_NAME)/*.o; \
    $(call return_done_for_module,"Cleaning for module $(APP_NAME) finished.")

configure: ensure_dir
	$(call return_done_for_module,"Configuring procedures for $(APP_NAME) finished")

ensure_dir:
	$(call ensure_dir,$(APP_DIR_EBIN))
	$(call ensure_dir,$(APP_DIR_BIN))
	$(call ensure_dir,$(APP_DIR_NIF))
	$(call ensure_dir,$(APP_DIR_BUILD))

ensure_dir_silent:
	$(call ensure_dir_silent,$(APP_DIR_EBIN))
	$(call ensure_dir_silent,$(APP_DIR_BIN))
	$(call ensure_dir_silent,$(APP_DIR_NIF))
	$(call ensure_dir_silent,$(APP_DIR_BUILD))

link_objects: build_objects
	$(COMPILLER_C) -o $(DIR_BIN)/$(APP_NAME)/a_time \
	 $(call dir_module_build,$(APP_NAME))/a_time.o \
	 $(call dir_module_build,$(APP_NAME))/a_time_handler.o \
	 || exit; \
	 $(call return_done_for_module,"Objects linked for module $(APP_NAME).")

build_objects: a_time.c a_time_handler.c
	$(call return_done_for_module,"Objects built for module $(APP_NAME).")

build_nifs: a_time_nif.so
	$(call return_done_for_module,"Erlang NIFs built for module $(APP_NAME).")

backup:
	rm -rf $(APP_DIR_BACKUP_EBIN)/*; \
	rm -rf $(APP_DIR_BACKUP_BIN)/*; \
	rm -rf $(APP_DIR_BACKUP_NIF)/*; \
	rm -rf $(APP_DIR_BACKUP_BUILD)/*; \
    $(call ensure_dir_silent,$(APP_DIR_BACKUP_EBIN)) \
    $(call ensure_dir_silent,$(APP_DIR_BACKUP_BIN)) \
    $(call ensure_dir_silent,$(APP_DIR_BACKUP_NIF)) \
    $(call ensure_dir_silent,$(APP_DIR_BACKUP_BUILD)) \
    cp -Rf $(APP_DIR_EBIN)/* $(APP_DIR_BACKUP_EBIN); \
    cp -Rf $(APP_DIR_BIN)/* $(APP_DIR_BACKUP_BIN); \
    cp -Rf $(APP_DIR_NIF)/* $(APP_DIR_BACKUP_NIF); \
    cp -Rf $(APP_DIR_BUILD)/* $(APP_DIR_BACKUP_BUILD); \
    $(call return_done_for_module,"Backup procedures for $(APP_NAME) finished.")


# ----------------------------------------------
# C modules

a_time.c:
	$(call build_o,a_time)

a_time_handler.c:
	$(call build_o,a_time_handler)


# ----------------------------------------------
# Erlang modules

a_time.erl:
	$(call build_erl,a_time)


# ----------------------------------------------
# Erlang NIF modules

a_time_nif.so:
	echo $(COMPILLER_NIF)
	$(COMPILLER_NIF) -o $(call dir_module_nif,$(APP_NAME))/a_time_nif.so \
	$(call dir_module_src,$(APP_NAME))/a_time_nif.c \
	$(call dir_module_src,$(APP_NAME))/a_time_handler.c \
	|| exit;