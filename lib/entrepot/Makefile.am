SUBDIRS = src tests
EXTRA_DIST = examples src tests
export app = entrepot
include ../../vars.mk
vertebra_conf_DATA = examples/$(app).cfg

dist-hook:
	mkdir $(distdir)/ebin
