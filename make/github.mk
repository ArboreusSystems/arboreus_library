# ----------------------------------------------
# @doc Arboreus publiher for Github
# (C) Arboreus library (http://arboreus.systems)
# (C) Alexandr Kirilov (http://alexandr.kirilov.me)
#
# ----------------------------------------------

.SILENT: publish

# ----------------------------------------------
# Targets

publish:
	rsync -avzh --del $(ROOT_DIR_MAKE) $(GITHUB_DIR)/; \
	rsync -zvh $(ROOT_DIR)/Makefile $(GITHUB_DIR)/; \
	echo "*** -----------------"; \
	echo "DONE! Syncronisation makefiles finished.\n"; \
	rsync -avzh --del $(ROOT_DIR_SRC) $(GITHUB_DIR)/; \
	echo "*** -----------------"; \
	echo "DONE! Syncronisation sources finished.\n"; \
	cd $(GITHUB_DIR); \
	git add -A; \
	echo "Enter the commit message: "; \
	read MESSAGE; \
	git commit -m "$$MESSAGE"; \
	git push; \
	echo "*** -----------------"; \
    echo "DONE! Publishing to Github finished.\n";