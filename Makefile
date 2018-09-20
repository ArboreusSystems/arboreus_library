# ----------------------------------------------
# Arboreus Erlang library make file
# (C) Arboreus library (http://arboreus.systems)
# (C) Alexandr Kirilov (http://alexandr.kirilov.me)
#
# ----------------------------------------------

.SILENT: all


# ----------------------------------------------
# Make's variables and extentions

include $(PWD)/make/variables.mk
include $(PWD)/make/action_primitives.mk
include $(PWD)/make/arguments.mk


# ----------------------------------------------
# Default targets

all:
	echo "Done! All targets executed."


# ----------------------------------------------
# Make's modules

include $(PWD)/make/compile.mk
include $(PWD)/make/paths.mk
include $(PWD)/make/backup.mk
include $(PWD)/make/configure.mk
include $(PWD)/make/github.mk

