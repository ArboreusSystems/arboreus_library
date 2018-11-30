# ----------------------------------------------
# Arboreus library make file
# (C) Arboreus library (http://arboreus.systems)
# (C) Alexandr Kirilov (http://alexandr.kirilov.me)
#
# ----------------------------------------------

MAKEFLAGS += --silent
.DEFAULT_GOAL: all


# ----------------------------------------------
# Variables and extentions

include $(PWD)/make/conf.mk
include $(PWD)/make/arguments.mk
include $(PWD)/make/actions.mk


# ----------------------------------------------
# Targets

all: configure set_arguments install clean

configure:
	$(call return_done_for_target,"Configuring procedures performed.")

install: check_arguments
	$(foreach MODULE,$(MODULES_FOR_ACTION),$(call module_install,$(MODULE)))
	$(call return_done_for_target,"Installing procedures performed.")

clean: check_arguments
	$(foreach MODULE,$(MODULES_FOR_ACTION),$(call module_clean,$(MODULE)))
	$(call return_done_for_target,"Cleaning procedures performed.")

package: install clean
	$(call return_done_for_target,"Package buiding procedures performed.")

check_arguments:
ifeq ($(words $(MODULES_FOR_ACTION)),0)
	$(eval MODULES_FOR_ACTION = $(MODULES))
endif

set_arguments:
	$(eval MODULES_FOR_ACTION = $(MODULES))