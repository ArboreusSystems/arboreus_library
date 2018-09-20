# ----------------------------------------------
# @doc Arboreus module's paths handler
# (C) Arboreus library (http://arboreus.systems)
# (C) Alexandr Kirilov (http://alexandr.kirilov.me)
#
# ----------------------------------------------

.SILENT: ensure_ebin ensure_ebin_all


# ----------------------------------------------
# Functions

# Ensure Ebin directory for module
define ensure_ebin_dir
$(call check_module_name,$(1))
$(call ensure_dir,$(call get_ebin_dir,$(1)))
endef


# ----------------------------------------------
# Targets

ensure_ebin:
	$(foreach MODULE,$(ARGUMENTS),$(call check_module_name,$(MODULE)))
	$(foreach TARGET,$(ARGUMENTS),$(call ensure_ebin_dir,$(TARGET)))
	$(call print_end,"Ensuring ebin direcories finished.\n")

ensure_ebin_all:
	$(foreach MODULE,$(MODULES),$(call ensure_ebin_dir,$(MODULE)))
	$(call print_end,"Ensuring all ebin direcories finished.\n")