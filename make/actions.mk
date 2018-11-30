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
# @doc Configure library module
# @params
#	$(1) - module name

define module_configure
make configure --directory $(call dir_module,$(1)) || exit;
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
$(COMPILLER_C) -o $(2) $(1) || exit;
echo "Done! Built C-object file: $(1)."
endef


# ----------------------------------------------
# @doc Compile Erlang file (*.erl) from source to specified directory
# @params
#	$(1) - full path to source file
#   $(2) - output directory

define build_erl
$(COMPILLER_ERL) -o $(2) $(1) || exit;
echo "Done! Built Erlang file: $(1)."
endef


# ----------------------------------------------
# @doc Copy Erlang app file (*.app) from source to specified directory
# @params
#	$(1) - full path to source file
#   $(2) - output directory

define build_app
cp $(1) $(2) || exit; \
echo "Done! Erlang app-file copied: $(1)."
endef

# ----------------------------------------------
# @doc Run target from receipe
# @params
#	$(1) - target

define target
@$(MAKE) -f $(THIS_FILE) $(1)
endef


# ----------------------------------------------
# @doc Ensure directory
# @params
#	$(1) - dir path

define ensure_dir
mkdir -p $(1); \
if [ -d $(1) ]; then \
echo "Done! Directory exists: $(1)"; \
else \
echo "Error! Directory not exists: $(1)"; \
exit; \
fi;
endef


# ----------------------------------------------
# @doc Ensure directory silent
# @params
#	$(1) - dir path

define ensure_dir_silent
mkdir -p $(1); \
if [ ! -d $(1) ]; then \
echo "Error! Directory not exists: $(1)"; \
exit; \
fi;
endef