# ----------------------------------------------
# @doc Arboreus configuration
# (C) Arboreus library (http://arboreus.systems)
# (C) Alexandr Kirilov (http://alexandr.kirilov.me)
#
# ----------------------------------------------

.SILENT: configure conf_erlang ensure_erlang_settings_file

# ----------------------------------------------
# Functions

# Ensure the code path in erlang settings file
define ensure_code_pathz
$(eval CODE_PATHZ := "$(call tmpl_code_path,$(1))")
$(eval CHECK_CODE_PATHZ := $(shell grep $(CODE_PATHZ) $(ERLANG_SETTINGS_FILE)))
if [ "$(CHECK_CODE_PATHZ)" == "" ]; then \
echo $(CODE_PATHZ) >> $(ERLANG_SETTINGS_FILE); \
echo "DONE! Path added for $(1)"; \
else \
echo "No action. Path for $(1) already added"; \
fi;
endef

# Return line within coe path adding
define tmpl_code_path
code:add_pathz(\"$(call get_ebin_dir,$(1))\").
endef

# Ensure existense of erlang settings file
define ensure_erlang_settings_file
if [ ! -f $(1) ]; then \
echo "%% ----------------------------------------------" >> $(1); \
echo "%% Generated automatically by Arboreus configurer" >> $(1); \
echo "%% $(shell date +'%y.%m.%d %H:%M:%S')" >> $(1); \
echo "%%" >> $(1); \
echo "%% ----------------------------------------------\n" >> $(1); \
fi; \
if [ -f $(1) ]; then \
echo "DONE! Erlang settings file $(1) existed."; \
else \
echo "ERROR! No erlang settings file: $(1)"; \
exit 1; \
fi;
endef


# ----------------------------------------------
# Targets

configure: conf_erlang
	$(call print_end,"Configuration finished.\n")

conf_erlang: ensure_erlang_settings_file
	$(foreach MODULE,$(MODULES),$(call ensure_code_pathz,$(MODULE)))

ensure_erlang_settings_file:
	$(call ensure_erlang_settings_file,$(ERLANG_SETTINGS_FILE))