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

include ../../make/directories.conf.mk
include ../../make/conf.mk
include ../../make/actions.mk


# ----------------------------------------------
# Variables

APP_NAME = a_test
APP_A_TIME = a_time
APP_UNIVERSAL_C = universal_c


# ----------------------------------------------
# Targets

all: install clean

install: backup clean ensure_dir_silent install_app install_erl
	$(call target,a_test.erl)
	$(call return_done_for_module,"Module $(APP_NAME) installed.")

install_app: link_objects
	$(call return_done_for_module,"Console application/s $(APP_NAME) installed.")

install_erl: a_test.erl
	$(call return_done_for_module,"Erlang modules/s and NIFs for $(APP_NAME) installed.")

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
	$(call ensure_dir_silent,$(APP_DIR_EBIN))
	$(call ensure_dir_silent,$(APP_DIR_BIN))
	$(call ensure_dir_silent,$(APP_DIR_BUILD))

link_objects: build_objects
	$(COMPILLER_C) -o $(DIR_BIN)/$(APP_NAME)/a_test \
	 $(call dir_module_build,$(APP_NAME))/a_test.o \
	 $(call dir_module_build,$(APP_NAME))/a_test_time.o \
	 $(call dir_module_build,$(APP_NAME))/a_test_time_ansi.o \
	 $(call dir_module_build,$(APP_NAME))/a_test_time_month.o \
	 $(call dir_module_build,$(APP_NAME))/a_test_time_rfc822.o \
	 $(call dir_module_build,$(APP_NAME))/a_test_time_rfc850.o \
	 $(call dir_module_build,$(APP_NAME))/a_test_time_weekday.o \
	 $(call dir_module_build,$(APP_NAME))/a_time_now_handler.o \
	 $(call dir_module_build,$(APP_NAME))/a_time_month.o \
	 $(call dir_module_build,$(APP_NAME))/a_time_weekday.o \
	 $(call dir_module_build,$(APP_NAME))/a_time_rfc822.o \
	 $(call dir_module_build,$(APP_NAME))/a_time_rfc850.o \
	 $(call dir_module_build,$(APP_NAME))/a_time_ansi.o \
	 $(call dir_module_build,$(APP_NAME))/a_convert.o \
	 $(call dir_module_build,$(APP_NAME))/a_string.o \
	 || exit; \
	 $(call return_done_for_module,"Objects linked for module $(APP_NAME).")

build_objects: a_test.c a_test_time.c a_test_time_ansi.c a_test_time_month.c a_test_time_rfc822.c a_test_time_rfc850.c \
a_test_time_rfc850.c a_test_time_weekday.c a_time_now_handler.c a_time_month.c a_time_weekday.c a_time_rfc822.c \
a_time_rfc850.c a_time_ansi.c a_convert.c a_string.c
	$(call return_done_for_module,"Objects built for module $(APP_NAME).")

backup:
	rm -rf $(APP_DIR_BACKUP_EBIN)/*; \
	rm -rf $(APP_DIR_BACKUP_BIN)/*; \
	rm -rf $(APP_DIR_BACKUP_BUILD)/*; \
    $(call ensure_dir_silent,$(APP_DIR_BACKUP_EBIN)) \
    $(call ensure_dir_silent,$(APP_DIR_BACKUP_BIN)) \
    $(call ensure_dir_silent,$(APP_DIR_BACKUP_BUILD)) \
    cp -Rf $(APP_DIR_BIN)/ $(APP_DIR_BACKUP_BIN); \
    cp -Rf $(APP_DIR_EBIN)/ $(APP_DIR_BACKUP_EBIN); \
    cp -Rf $(APP_DIR_BUILD)/ $(APP_DIR_BACKUP_BUILD); \
    $(call return_done_for_module,"Backup procedures for $(APP_NAME) finished.")


# ----------------------------------------------
# C modules

a_test.c:
	$(call build_o,a_test)

# a_test_time

a_test_time.c:
	$(call build_o_by_path,$(call dir_module_src,$(APP_NAME))/a_time/a_test_time.c,$(call dir_module_build,$(APP_NAME))/a_test_time.o)

a_test_time_ansi.c:
	$(call build_o_by_path,$(call dir_module_src,$(APP_NAME))/a_time/a_test_time_ansi.c,$(call dir_module_build,$(APP_NAME))/a_test_time_ansi.o)

a_test_time_month.c:
	$(call build_o_by_path,$(call dir_module_src,$(APP_NAME))/a_time/a_test_time_month.c,$(call dir_module_build,$(APP_NAME))/a_test_time_month.o)

a_test_time_rfc822.c:
	$(call build_o_by_path,$(call dir_module_src,$(APP_NAME))/a_time/a_test_time_rfc822.c,$(call dir_module_build,$(APP_NAME))/a_test_time_rfc822.o)

a_test_time_rfc850.c:
	$(call build_o_by_path,$(call dir_module_src,$(APP_NAME))/a_time/a_test_time_rfc850.c,$(call dir_module_build,$(APP_NAME))/a_test_time_rfc850.o)

a_test_time_weekday.c:
	$(call build_o_by_path,$(call dir_module_src,$(APP_NAME))/a_time/a_test_time_weekday.c,$(call dir_module_build,$(APP_NAME))/a_test_time_weekday.o)

# a_time

a_time_now_handler.c:
	$(call build_o_by_path,$(call dir_module_src,$(APP_A_TIME))/a_time_now_handler.c,$(call dir_module_build,$(APP_NAME))/a_time_now_handler.o)

a_time_month.c:
	$(call build_o_by_path,$(call dir_module_src,$(APP_A_TIME))/a_time_month.c,$(call dir_module_build,$(APP_NAME))/a_time_month.o)

a_time_weekday.c:
	$(call build_o_by_path,$(call dir_module_src,$(APP_A_TIME))/a_time_weekday.c,$(call dir_module_build,$(APP_NAME))/a_time_weekday.o)

a_time_rfc822.c:
	$(call build_o_by_path,$(call dir_module_src,$(APP_A_TIME))/a_time_rfc822.c,$(call dir_module_build,$(APP_NAME))/a_time_rfc822.o)

a_time_rfc850.c:
	$(call build_o_by_path,$(call dir_module_src,$(APP_A_TIME))/a_time_rfc850.c,$(call dir_module_build,$(APP_NAME))/a_time_rfc850.o)

a_time_ansi.c:
	$(call build_o_by_path,$(call dir_module_src,$(APP_A_TIME))/a_time_ansi.c,$(call dir_module_build,$(APP_NAME))/a_time_ansi.o)

# universal_c

a_convert.c:
	$(call build_o_by_path,$(call dir_module_src,$(APP_UNIVERSAL_C))/a_convert.c,$(call dir_module_build,$(APP_NAME))/a_convert.o)

a_string.c:
	$(call build_o_by_path,$(call dir_module_src,$(APP_UNIVERSAL_C))/a_string.c,$(call dir_module_build,$(APP_NAME))/a_string.o)


# ----------------------------------------------
# Erlang modules

a_test.erl:
	$(call build_erl,a_test)