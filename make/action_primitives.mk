# ----------------------------------------------
# @doc Arboreus make file actions primitives
# (C) Arboreus library (http://arboreus.systems)
# (C) Alexandr Kirilov (http://alexandr.kirilov.me)
#
# ----------------------------------------------


# ----------------------------------------------
# @doc Check the module name
# @params
#	$(1) - module name

define check_module_name
if [ "$(findstring $(1),$(MODULES))" == "" ]; then \
echo "ERROR! Wrong module name: $(1)."; \
exit 1; \
fi;
endef


# ----------------------------------------------
# @doc Get module ebin directory
# @params
# 	$(1) - module name

define get_ebin_dir
$(ROOT_DIR_EBIN)/$(1)
endef


# ----------------------------------------------
# @doc Get module backup directory
# @params
# 	$(1) - module name

define get_backup_dir
$(ROOT_DIR_BACKUP)/$(1)
endef


# ----------------------------------------------
# @doc Get module src directory
# @params
# 	$(1) - module name

define get_src_dir
$(ROOT_DIR_SRC)/$(1)
endef


# ----------------------------------------------
# @doc Ensure directory existense
# @params
# 	$(1) - directory path

define ensure_dir
if [ ! -d $(1) ]; then \
mkdir -p $(1); \
fi; \
if [ -d $(1) ]; then \
echo "DONE! Directory existed: $(1)"; \
else \
echo "ERROR! Directory not existed: $(1)"; \
exit 1; \
fi;
endef


# ----------------------------------------------
# @doc Copy directory
# @params
# 	$(1) - source path
#	$(2) - destination path

define copy_dir
$(eval SOURCE := $(1)) \
$(eval DESTINATION := $(2)) \
if [ ! -d $(SOURCE) ]; then \
echo "No action. Nothing for copy: $(SOURCE)";\
else \
if [ -d $(DESTINATION) ]; then \
$(call delete_dir,$(DESTINATION)) \
fi; \
cp -av -R $(SOURCE) $(DESTINATION); \
if [ ! -d $(DESTINATION) ]; then \
echo "ERROR! Backup failed:$(1)"; \
exit 1; \
else \
echo "DONE! Copied for backup:"; \
echo "\tSource path: $(SOURCE)"; \
echo "\tDestination path: $(DESTINATION)"; \
fi; \
fi;
endef


# ----------------------------------------------
# @doc Delete directory
# @params
#	$(1) - directory path

define delete_dir
rm -rf $(1); \
if [ -d $(1) ]; then \
echo "ERROR! Directory not deleted: $(1)"; \
exit 1; \
fi;
endef


# ----------------------------------------------
# Move directory
# @params
# 	$(1) - source path
#	$(2) - destination path

define move_dir
$(eval SOURCE := $(1)) \
$(eval DESTINATION := $(2)) \
if [ ! -d $(SOURCE) ]; then \
echo "No action. Nothing for copy: $(SOURCE)";\
else \
if [ -d $(DESTINATION) ]; then \
$(call delete_dir,$(DESTINATION)) \
fi; \
mv $(SOURCE) $(DESTINATION); \
if [ ! -d $(DESTINATION) ]; then \
echo "ERROR! Moving failed:$(1)"; \
exit 1; \
else \
echo "DONE! Moving for backup:"; \
echo "\tSource path: $(SOURCE)"; \
echo "\tDestination path: $(DESTINATION)"; \
fi; \
fi;
endef


# ----------------------------------------------
# Print action's end notice
# @params
# 	$(1) - end notice text

define print_end
echo "*** -----------------"; \
echo $(1);
endef