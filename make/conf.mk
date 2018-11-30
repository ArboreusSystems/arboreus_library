# ----------------------------------------------
# Arboreus make file configuration
# (C) Arboreus library (http://arboreus.systems)
# (C) Alexandr Kirilov (http://alexandr.kirilov.me)
#
# ----------------------------------------------


# ----------------------------------------------
# Arboreus Library modules list

MODULES += a_balancer
MODULES += a_event_storage
MODULES += a_loger
MODULES += a_numbers
MODULES += a_structure
MODULES += a_structure_tree
MODULES += a_test
MODULES += a_users
MODULES += handlers
MODULES += universal
MODULES += yaws_addons


# ----------------------------------------------
# Arboreus Library directory structure

DIR = $(PWD)
DIR_BIN = $(PWD)/bin
DIR_EBIN = $(PWD)/ebin
DIR_LIB = $(PWD)/lib
DIR_MAKE = $(PWD)/make
DIR_BUILD = $(PWD)/build
DIR_BACKUP = $(PWD)/backup
DIR_BACKUP_BIN = $(DIR_BACKUP)/bin
DIR_BACKUP_EBIN = $(DIR_BACKUP)/ebin


# ----------------------------------------------
# Arboreus Applications settings

APP_DIR_BUILD = $(DIR_BUILD)/$(APP_NAME)


# ----------------------------------------------
# Compiler mode

COMPILLER_C = clang -Werror
COMPILLER_ERL = erlc
COMPILLER_ERL_MODE = no_warning
