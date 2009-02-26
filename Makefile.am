SUBDIRS = lib

bin_SCRIPTS = bin/vertebractl
vertebra_conf_DATA = conf/vertebractl.conf
include vars.mk

CLEANFILES = $(bin_SCRIPTS) $(vertebra_conf_DATA)
EXTRA_DIST = conf src

export ERL_LIBS = $(abs_srcdir)/lib

bin:
	mkdir bin

bin/vertebractl: bin src/vertebractl.in Makefile
	$(do_subst) < $(srcdir)/src/vertebractl.in > $@
	chmod +x $@

conf/vertebractl.conf: conf/vertebractl.conf.in Makefile
	$(do_subst) < $(srcdir)/conf/vertebractl.conf.in > $@

initdb: agents = entrepot herault cavalcade
initdb: bin/vertebractl
	bin/initdb $(agents)
