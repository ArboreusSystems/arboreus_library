# ----------------------------------------------
# @doc Arboreus make file actions
# (C) Arboreus library (http://arboreus.systems)
# (C) Alexandr Kirilov (http://alexandr.kirilov.me)
#
# ----------------------------------------------


# ----------------------------------------------
# @doc Return "Done!" message
# @params
#	$(1) - message

define return_done_for_target
echo "----------------------------"; \
echo "Done! $(1)\n";
endef


# ----------------------------------------------
# @doc Return "Done!" message for module
# @params
#	$(1) - message

define return_done_for_module
echo "*** $(1)\n";
endef


# ----------------------------------------------
# @doc Install library module
# @params
#	$(1) - module name

define module_install
make install --directory $(call dir_module,$(1)) || exit;
endef


# ----------------------------------------------
# @doc Clean library module
# @params
#	$(1) - module name

define module_clean
make clean --directory $(call dir_module,$(1)) || exit;
endef


# ----------------------------------------------
# @doc Return libray module root directory
# @params
#	$(1) - module name

define dir_module
$(DIR_LIB)/$(1)
endef


# ----------------------------------------------
# @doc Return libray module source files directory
# @params
#	$(1) - module name

define dir_module_src
$(call dir_module,$(1))/src
endef


# ----------------------------------------------
# @doc Return libray module build directory
# @params
#	$(1) - module name

define dir_module_build
$(DIR_BUILD)/$(1)
endef


# ----------------------------------------------
# @doc Build object file (*.o) from source to specified directory
# @params
#	$(1) - module name
#   $(2) - application name

define build_o
$(eval SOURCE = $(call dir_module_src,$(2))/$(1).c)
$(eval OUTPUT = $(call dir_module_build,$(2))/$(1).o)
$(COMPILLER_C) -c -o $(OUTPUT) $(SOURCE) || exit; \
echo "Done! File built: $(1).o";
endef


# ----------------------------------------------
# @doc Compile object file (*.o) from source to specified directory
# @params
#	$(1) - full path to source file
#   $(2) - full path to output file

define build_c
$(COMPILLER_C) -o $(2) $(1);
endef


# ----------------------------------------------
# @doc Run target from receipe
# @params
#	$(1) - target

define target
@$(MAKE) -f $(THIS_FILE) $(1)
endef