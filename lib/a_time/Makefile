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

include ../../make/directories.conf.mk
include ../../make/conf.mk
include ../../make/actions.mk


# ----------------------------------------------
# Variables

APP_NAME = a_time
APP_UNIVERSAL_C = universal_c


# ----------------------------------------------
# Targets

all: install clean

install: backup clean ensure_dir_silent install_app install_erl
	$(call return_done_for_module,"Module $(APP_NAME) installed.")

install_app: link_objects
	$(call return_done_for_module,"Console application/s $(APP_NAME) installed.")

install_erl: build_nifs build_erl
	$(call return_done_for_module,"Erlang modules/s and NIFs for $(APP_NAME) installed.")

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
	$(COMPILLER_C) -o $(DIR_BIN)/$(APP_NAME)/aTimeNow \
	 $(call dir_module_build,$(APP_NAME))/aTimeNow.o \
	 $(call dir_module_build,$(APP_NAME))/aTimeNowHandler.o \
	 $(call dir_module_build,$(APP_NAME))/aTimeMonth.o \
	 $(call dir_module_build,$(APP_NAME))/aTimeWeekday.o \
	 $(call dir_module_build,$(APP_NAME))/aTimeRFC822.o \
	 $(call dir_module_build,$(APP_NAME))/aTimeRFC850.o \
	 $(call dir_module_build,$(APP_NAME))/aTimeANSI.o \
	 $(call dir_module_build,$(APP_NAME))/aConvert.o \
	 $(call dir_module_build,$(APP_NAME))/aString.o \
	 || exit; \
	 $(call return_done_for_module,"Objects linked for module $(APP_NAME).")

build_objects: aTimeNow.c aTimeNowHandler.c aTimeMonth.c aTimeWeekday.c aTimeRFC822.c \
aTimeRFC850.c aTimeANSI.c aConvert.c aString.c
	$(call return_done_for_module,"Objects built for module $(APP_NAME).")

build_nifs: aTimeNowNIF.so
	$(call return_done_for_module,"Erlang NIFs built for $(APP_NAME).")

build_erl: a_time_now.erl a_time.erl
	$(call return_done_for_module,"Erlang modules built for $(APP_NAME).")

backup:
	rm -rf $(APP_DIR_BACKUP_EBIN)/*; \
	rm -rf $(APP_DIR_BACKUP_BIN)/*; \
	rm -rf $(APP_DIR_BACKUP_NIF)/*; \
	rm -rf $(APP_DIR_BACKUP_BUILD)/*; \
    $(call ensure_dir_silent,$(APP_DIR_BACKUP_EBIN)) \
    $(call ensure_dir_silent,$(APP_DIR_BACKUP_BIN)) \
    $(call ensure_dir_silent,$(APP_DIR_BACKUP_NIF)) \
    $(call ensure_dir_silent,$(APP_DIR_BACKUP_BUILD)) \
    cp -Rf $(APP_DIR_EBIN)/ $(APP_DIR_BACKUP_EBIN); \
    cp -Rf $(APP_DIR_BIN)/ $(APP_DIR_BACKUP_BIN); \
    cp -Rf $(APP_DIR_NIF)/ $(APP_DIR_BACKUP_NIF); \
    cp -Rf $(APP_DIR_BUILD)/ $(APP_DIR_BACKUP_BUILD); \
    $(call return_done_for_module,"Backup procedures for $(APP_NAME) finished.")


# ----------------------------------------------
# C modules

aTimeNow.c:
	$(call build_o,aTimeNow)

aTimeNowHandler.c:
	$(call build_o,aTimeNowHandler)

aTimeMonth.c:
	$(call build_o,aTimeMonth)

aTimeWeekday.c:
	$(call build_o,aTimeWeekday)

aTimeRFC822.c:
	$(call build_o,aTimeRFC822)

aTimeRFC850.c:
	$(call build_o,aTimeRFC850)

aTimeANSI.c:
	$(call build_o,aTimeANSI)

aConvert.c:
	$(call build_o_by_path,$(call dir_module_src,$(APP_UNIVERSAL_C))/aConvert.c,$(call dir_module_build,$(APP_NAME))/aConvert.o)

aString.c:
	$(call build_o_by_path,$(call dir_module_src,$(APP_UNIVERSAL_C))/aString.c,$(call dir_module_build,$(APP_NAME))/aString.o)


# ----------------------------------------------
# Erlang modules

a_time_now.erl:
	$(call build_erl,a_time_now)

a_time.erl:
	$(call build_erl,a_time)


# ----------------------------------------------
# Erlang NIF modules

aTimeNowNIF.so:
	$(COMPILLER_NIF) -o $(call dir_module_nif,$(APP_NAME))/aTimeNowNIF.so \
    $(call dir_module_src,$(APP_NAME))/aTimeNowNIF.c \
    $(call dir_module_src,$(APP_NAME))/aTimeNowHandler.c \
    $(call dir_module_src,$(APP_NAME))/aTimeWeekday.c \
    $(call dir_module_src,$(APP_NAME))/aTimeMonth.c \
    $(call dir_module_src,$(APP_NAME))/aTimeRFC850.c \
    $(call dir_module_src,$(APP_NAME))/aTimeRFC822.c \
    $(call dir_module_src,$(APP_NAME))/aTimeANSI.c \
    $(call dir_module_src,$(APP_UNIVERSAL_C))/aConvert.c \
    $(call dir_module_src,$(APP_UNIVERSAL_C))/aString.c \
    || exit;