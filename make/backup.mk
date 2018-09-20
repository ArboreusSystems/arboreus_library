# ----------------------------------------------
# @doc Arboreus makefile backup handler
# (C) Arboreus library (http://arboreus.systems)
# (C) Alexandr Kirilov (http://alexandr.kirilov.me)
#
# ----------------------------------------------

.SILENT: backup backup_all backup_restore backup_restore_all

# ----------------------------------------------
# Functions

# Moving directory
define move_module
$(call ensure_dir,$(ROOT_DIR_BACKUP))
$(call move_dir,$(call get_ebin_dir,$(1)),$(ROOT_DIR_BACKUP)/$(1))
endef

# Restoring directory
define restore_module
$(call ensure_dir,$(ROOT_DIR_EBIN))
$(call copy_dir,$(ROOT_DIR_BACKUP)/$(1),$(call get_module_dir,$(1)))
endef

# ----------------------------------------------
# Targets

backup:
	$(foreach MODULE,$(ARGUMENTS),$(call check_module_name,$(MODULE)))
	$(foreach MODULE,$(ARGUMENTS),$(call move_module,$(MODULE)))
	$(call print_end,"Backup moving modules finished.\n")

backup_all:
	$(foreach MODULE,$(MODULES),$(call move_module,$(MODULE)))
	$(call print_end,"Backup moving all modules finished.\n")

backup_restore:
	$(foreach MODULE,$(ARGUMENTS),$(call check_module_name,$(MODULE)))
	$(foreach MODULE,$(ARGUMENTS),$(call restore_module,$(MODULE)))
	$(call print_end,"Restoring modules finished.\n")

backup_restore_all:
	$(foreach MODULE,$(MODULES),$(call restore_module,$(MODULE)))
	$(call print_end,"Restoring modules finished.\n")