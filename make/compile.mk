# ----------------------------------------------
# @doc Arboreus erlang compilation handler
# (C) Arboreus library (http://arboreus.systems)
# (C) Alexandr Kirilov (http://alexandr.kirilov.me)
#
# ----------------------------------------------

.SILENT: compile compile_all


# ----------------------------------------------
# Functions

# Compile Erlang file
define compile_file
erlc -W2 -o $(call get_ebin_dir,$(2)) $(1); \
echo "DONE! Module compiled: $(1)";
endef

# Compile Arboreus module
define compile_module
$(eval MODULE_NAME := $(1))
cd $(call get_src_dir,$(MODULE_NAME)); \
$(foreach FILE_NAME,$(shell find $(call get_src_dir,$(MODULE_NAME)) -name '*.erl'),$(call compile_file,$(FILE_NAME),$(MODULE_NAME)))
if echo "$(MODULE_APP)" | grep -q "$(MODULE_NAME)"; then \
cp $(call get_src_dir,$(MODULE_NAME))/$(1).app $(call get_ebin_dir,$(MODULE_NAME))/; \
echo "DONE! Application resource file $(MODULE_NAME).app copied."; \
fi; \
echo "*** Module $(1) compilation finished\n";
endef


# ----------------------------------------------
# Targets

compile: backup ensure_ebin
	$(foreach MODULE,$(ARGUMENTS),$(call compile_module,$(MODULE)))
	$(call print_end,"Compiling requested modules finished.\n")

compile_all: backup_all ensure_ebin_all
	$(foreach MODULE,$(MODULES),$(call compile_module,$(MODULE)))
	$(call print_end,"Compiling all modules finished.\n")