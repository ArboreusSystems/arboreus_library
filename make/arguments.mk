# ----------------------------------------------
# @doc Arboreus shell arguments handling
# (C) Arboreus library (http://arboreus.systems)
# (C) Alexandr Kirilov (http://alexandr.kirilov.me)
#
# ----------------------------------------------


# ----------------------------------------------
# Setting modules for action for target "configure"

ifeq (configure, $(firstword $(MAKECMDGOALS)))
MODULES_FOR_ACTION := $(wordlist 2, $(words $(MAKECMDGOALS)), $(MAKECMDGOALS))
$(eval $(MODULES_FOR_ACTION):;@true)
endif


# ----------------------------------------------
# Setting modules for action for target "install"

ifeq (install, $(firstword $(MAKECMDGOALS)))
MODULES_FOR_ACTION := $(wordlist 2, $(words $(MAKECMDGOALS)), $(MAKECMDGOALS))
$(eval $(MODULES_FOR_ACTION):;@true)
endif


# ----------------------------------------------
# Setting modules for action for target "clean"

ifeq (clean, $(firstword $(MAKECMDGOALS)))
MODULES_FOR_ACTION := $(wordlist 2, $(words $(MAKECMDGOALS)), $(MAKECMDGOALS))
$(eval $(MODULES_FOR_ACTION):;@true)
endif