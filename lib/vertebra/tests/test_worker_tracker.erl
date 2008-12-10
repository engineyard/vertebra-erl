-module(test_worker_tracker).

-author("ksmith@engineyard.com").

-include_lib("eunit/include/eunit.hrl").

-define(SINGLE_EL, {xmlelement, "op", [{"token", "abc"}], []}).
-define(MULTI_EL, [{xmlelement, "error", [{"type", "wait"}], []}, ?SINGLE_EL]).

store_test_() ->
  {setup,
   fun() ->
       {ok, Pid} = gen_actor_tracker:start_link(),
       register(tracker, Pid),
       Pid end,
   fun(Pid) ->
       exit(Pid, normal) end,
   [fun() ->
        gen_actor_tracker:register_request_worker(whereis(tracker), "abc", self()) end,
   fun() ->
       gen_actor_tracker:register_request_worker(whereis(tracker), "abc", self()),
       gen_actor_tracker:unregister_request_worker(whereis(tracker), "abc"),
       gen_actor_tracker:register_request_worker(whereis(tracker), "abc", self()),
       gen_actor_tracker:unregister_request_worker(whereis(tracker), self()) end,
    fun() ->
        Me = self(),
        gen_actor_tracker:register_request_worker(whereis(tracker), "abc", Me),
        {ok, Me} = gen_actor_tracker:find_worker_for_token(whereis(tracker), "abc") end,
    fun() ->
        gen_actor_tracker:register_request_worker(whereis(tracker), "abc", self()),
        gen_actor_tracker:unregister_request_worker(whereis(tracker), self()),
        not_found = gen_actor_tracker:find_worker_for_token(whereis(tracker), "abc") end,
    fun() ->
        Me = self(),
        gen_actor_tracker:register_request_worker(whereis(tracker), "abc", Me),
        {ok, Me} = gen_actor_tracker:find_worker_for_stanza(whereis(tracker), ?SINGLE_EL) end,
    fun() ->
        Me = self(),
        gen_actor_tracker:register_request_worker(whereis(tracker), "abc", Me),
        {ok, Me} = gen_actor_tracker:find_worker_for_stanza(whereis(tracker), ?MULTI_EL) end,
    fun() ->
        Me = self(),
        gen_actor_tracker:unregister_request_worker(whereis(tracker), self()),
        gen_actor_tracker:register_request_worker(whereis(tracker), "def", Me),
        not_found= gen_actor_tracker:find_worker_for_stanza(whereis(tracker), ?SINGLE_EL) end,
    fun() ->
        Me = self(),
        gen_actor_tracker:unregister_request_worker(whereis(tracker), self()),
        gen_actor_tracker:register_request_worker(whereis(tracker), "def", Me),
        not_found= gen_actor_tracker:find_worker_for_stanza(whereis(tracker), ?MULTI_EL) end]}.
