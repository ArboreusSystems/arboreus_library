# ----------------------------------------------
# @doc Arboreus module's tests handler
# (C) Arboreus library (http://arboreus.systems)
# (C) Alexandr Kirilov (http://alexandr.kirilov.me)
#
# ----------------------------------------------

.SILENT: test

# ----------------------------------------------
# Functions

# ----------------------------------------------
# Targets

test: compile_all
	erl -name test -run a_test run -run init stop