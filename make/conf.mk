# ----------------------------------------------
# Arboreus makefile configuration
# (C) Arboreus library (http://arboreus.systems)
# (C) Alexandr Kirilov (http://alexandr.kirilov.me)
#
# ----------------------------------------------


# ----------------------------------------------
# Operation System Type

UNAME := $(shell uname)


# ----------------------------------------------
# Arboreus Library modules list

MODULES += a_balancer
MODULES += a_event_storage
MODULES += a_loger
MODULES += a_numbers
MODULES += a_structure
MODULES += a_structure_tree
MODULES += a_test
MODULES += a_time
MODULES += a_users
MODULES += handlers
MODULES += universal
MODULES += yaws_addons


# ----------------------------------------------
# Arboreus Library directory structure

DIR = $(PWD)
DIR_BIN = $(PWD)/bin
DIR_NIF = $(PWD)/nif
DIR_EBIN = $(PWD)/ebin
DIR_LIB = $(PWD)/lib
DIR_MAKE = $(PWD)/make
DIR_BUILD = $(PWD)/build
DIR_BACKUP = $(PWD)/backup
DIR_BACKUP_BIN = $(DIR_BACKUP)/bin
DIR_BACKUP_NIF = $(DIR_BACKUP)/nif
DIR_BACKUP_EBIN = $(DIR_BACKUP)/ebin


# ----------------------------------------------
# Arboreus Applications settings

APP_DIR_BUILD = $(DIR_BUILD)/$(APP_NAME)
APP_DIR_SRC = $(DIR_LIB)/$(APP_NAME)/src
APP_DIR_EBIN = $(DIR_EBIN)/$(APP_NAME)
APP_DIR_BIN = $(DIR_BIN)/$(APP_NAME)
APP_DIR_NIF = $(DIR_NIF)/$(APP_NAME)
APP_DIR_BACKUP_BIN = $(DIR_BACKUP)/bin/$(APP_NAME)
APP_DIR_BACKUP_NIF = $(DIR_BACKUP)/nif/$(APP_NAME)
APP_DIR_BACKUP_EBIN = $(DIR_BACKUP)/ebin/$(APP_NAME)
APP_DIR_BACKUP_BUILD = $(DIR_BACKUP)/build/$(APP_NAME)


# ----------------------------------------------
# Compiler mode

COMPILLER_C = clang -Werror
COMPILLER_ERL = erlc -Werror
ifeq ($(UNAME),Darwin)
COMPILLER_NIF = clang -fpic -Werror -shared -undefined dynamic_lookup
else
COMPILLER_NIF = clang -fpic -Werror -shared
endif