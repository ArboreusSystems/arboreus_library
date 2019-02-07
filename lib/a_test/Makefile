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
	$(COMPILLER_C) -o $(DIR_BIN)/$(APP_NAME)/aTest \
	 $(call dir_module_build,$(APP_NAME))/aTest.o \
	 $(call dir_module_build,$(APP_NAME))/aTestTime.o \
	 $(call dir_module_build,$(APP_NAME))/aTestTimeANSI.o \
	 $(call dir_module_build,$(APP_NAME))/aTestTimeMonth.o \
	 $(call dir_module_build,$(APP_NAME))/aTestTimeRFC822.o \
	 $(call dir_module_build,$(APP_NAME))/aTestTimeRFC850.o \
	 $(call dir_module_build,$(APP_NAME))/aTestTimeWeekday.o \
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

build_objects: aTest.c aTestTime.c aTestTimeANSI.c aTestTimeMonth.c aTestTimeRFC822.c aTestTimeRFC850.c \
aTestTimeRFC850.c aTestTimeWeekday.c aTimeNowHandler.c aTimeMonth.c aTimeWeekday.c aTimeRFC822.c \
aTimeRFC850.c aTimeANSI.c aConvert.c aString.c
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

aTest.c:
	$(call build_o,aTest)

# a_test_time

aTestTime.c:
	$(call build_o_by_path,$(call dir_module_src,$(APP_NAME))/a_time/aTestTime.c,$(call dir_module_build,$(APP_NAME))/aTestTime.o)

aTestTimeANSI.c:
	$(call build_o_by_path,$(call dir_module_src,$(APP_NAME))/a_time/aTestTimeANSI.c,$(call dir_module_build,$(APP_NAME))/aTestTimeANSI.o)

aTestTimeMonth.c:
	$(call build_o_by_path,$(call dir_module_src,$(APP_NAME))/a_time/aTestTimeMonth.c,$(call dir_module_build,$(APP_NAME))/aTestTimeMonth.o)

aTestTimeRFC822.c:
	$(call build_o_by_path,$(call dir_module_src,$(APP_NAME))/a_time/aTestTimeRFC822.c,$(call dir_module_build,$(APP_NAME))/aTestTimeRFC822.o)

aTestTimeRFC850.c:
	$(call build_o_by_path,$(call dir_module_src,$(APP_NAME))/a_time/aTestTimeRFC850.c,$(call dir_module_build,$(APP_NAME))/aTestTimeRFC850.o)

aTestTimeWeekday.c:
	$(call build_o_by_path,$(call dir_module_src,$(APP_NAME))/a_time/aTestTimeWeekday.c,$(call dir_module_build,$(APP_NAME))/aTestTimeWeekday.o)

# a_time

aTimeNowHandler.c:
	$(call build_o_by_path,$(call dir_module_src,$(APP_A_TIME))/aTimeNowHandler.c,$(call dir_module_build,$(APP_NAME))/aTimeNowHandler.o)

aTimeMonth.c:
	$(call build_o_by_path,$(call dir_module_src,$(APP_A_TIME))/aTimeMonth.c,$(call dir_module_build,$(APP_NAME))/aTimeMonth.o)

aTimeWeekday.c:
	$(call build_o_by_path,$(call dir_module_src,$(APP_A_TIME))/aTimeWeekday.c,$(call dir_module_build,$(APP_NAME))/aTimeWeekday.o)

aTimeRFC822.c:
	$(call build_o_by_path,$(call dir_module_src,$(APP_A_TIME))/aTimeRFC822.c,$(call dir_module_build,$(APP_NAME))/aTimeRFC822.o)

aTimeRFC850.c:
	$(call build_o_by_path,$(call dir_module_src,$(APP_A_TIME))/aTimeRFC850.c,$(call dir_module_build,$(APP_NAME))/aTimeRFC850.o)

aTimeANSI.c:
	$(call build_o_by_path,$(call dir_module_src,$(APP_A_TIME))/aTimeANSI.c,$(call dir_module_build,$(APP_NAME))/aTimeANSI.o)

# universal_c

aConvert.c:
	$(call build_o_by_path,$(call dir_module_src,$(APP_UNIVERSAL_C))/aConvert.c,$(call dir_module_build,$(APP_NAME))/aConvert.o)

aString.c:
	$(call build_o_by_path,$(call dir_module_src,$(APP_UNIVERSAL_C))/aString.c,$(call dir_module_build,$(APP_NAME))/aString.o)


# ----------------------------------------------
# Erlang modules

a_test.erl:
	$(call build_erl,a_test)