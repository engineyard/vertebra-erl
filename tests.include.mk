erl_files = $(wildcard *.erl)
beam_files = $(erl_files:%.erl=../ebin/%.beam)
check_DATA = $(beam_files)

CLEANFILES = $(check_DATA)

ERLCFLAGS = @ERLCFLAGS@ +debug_info

check-local:
	$(ERL) -pa ../ebin -eval "test_suite:test()" -s init stop -noshell

../ebin/%.beam: %.erl
	$(ERLC) $(ERLCFLAGS) -b beam -I ../include -I ../src -o ../ebin $<
