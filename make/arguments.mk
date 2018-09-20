# ----------------------------------------------
# @doc Arboreus shel arguments handler
# (C) Arboreus library (http://arboreus.systems)
# (C) Alexandr Kirilov (http://alexandr.kirilov.me)
#
# ----------------------------------------------


# Ebin directories
ifeq (ensure_ebin, $(firstword $(MAKECMDGOALS)))
  ARGUMENTS := $(wordlist 2, $(words $(MAKECMDGOALS)), $(MAKECMDGOALS))
  $(eval $(ARGUMENTS):;@true)
endif

# Compile
ifeq (compile, $(firstword $(MAKECMDGOALS)))
  ARGUMENTS := $(wordlist 2, $(words $(MAKECMDGOALS)), $(MAKECMDGOALS))
  $(eval $(ARGUMENTS):;@true)
endif