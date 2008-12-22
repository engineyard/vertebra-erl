appdir = $(vertebra_libdir)/$(app)-$(VERSION)
ebindir = $(appdir)/ebin

erl_files = $(wildcard *.erl)
hrl_files = $(wildcard *.hrl)
beam_files = $(erl_files:%.erl=../ebin/%.beam)
app_files = ../ebin/$(app).app
ebin_DATA = $(beam_files) $(app_files)

CLEANFILES = $(ebin_DATA)
EXTRA_DIST = $(erl_files) $(hrl_files) $(app).app.in

ERLCFLAGS = @ERLCFLAGS@ +debug_info

../ebin/%.beam: %.erl
	$(ERLC) $(ERLCFLAGS) -b beam -I ../include -o ../ebin $<

../ebin/$(app).app: modules = $(erl_files:%.erl=%,)
../ebin/$(app).app: $(app).app.in
	$(do_subst) -e 's|[@]modules[@]|$(modules)|g' -e 's/,]}/]}/' \
	< $(app).app.in > $@
