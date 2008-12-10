-module(gen_actor_tracker).

-author("ksmith@engineyard.com").

-behaviour(gen_server).
-behaviour(gen_xmpp_worker_tracker).

%% API
-export([start_link/0, register_request_worker/3, unregister_request_worker/2]).
-export([find_worker_for_token/2, find_worker_for_stanza/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {workers=dict:new()}).

%% Public API functions

start_link() ->
  gen_server:start_link(?MODULE, [], []).

register_request_worker(ServerPid, Token, WorkerPid) when is_list(Token), is_pid(WorkerPid) ->
  gen_server:cast(ServerPid, {register_request_worker, Token, WorkerPid}).

unregister_request_worker(ServerPid, WorkerPid) when is_pid(WorkerPid) ->
  gen_server:cast(ServerPid, {unregister_worker, WorkerPid});

unregister_request_worker(ServerPid, Token) when is_list(Token) ->
  gen_server:cast(ServerPid, {unregister_worker, Token}).

find_worker_for_token(ServerPid, Token) when is_list(Token) ->
  gen_server:call(ServerPid, {find_worker, Token}).

find_worker_for_stanza(ServerPid, Stanza) ->
  gen_server:call(ServerPid, {find_stanza_worker, Stanza}).

%% gen_server callback functions

init([]) ->
  {ok, #state{}}.

handle_call({find_stanza_worker, {xmlelement, _, Attrs, _}}, _From, State) ->
  case proplists:get_value("token", Attrs) of
    undefined ->
      {reply, not_found, State};
    Token ->
      {reply, lookup_token(Token, State), State}
  end;
handle_call({find_stanza_worker, Stanzas}, _From, State) when is_list(Stanzas) ->
  case find_token(Stanzas) of
    undefined ->
      {reply, not_found, State};
    Token ->
      {reply, lookup_token(Token, State), State}
  end;

handle_call({find_worker, Token}, _From, State) ->
  {reply, lookup_token(Token, State), State};

handle_call(_Request, _From, State) ->
  {reply, ignored, State}.

handle_cast({register_request_worker, Token, WorkerPid}, State) ->
  {noreply, store_state(State, Token, WorkerPid)};

handle_cast({unregister_worker, Key}, State) when is_pid(Key) ->
  {noreply, erase_pid(State, Key)};

handle_cast({unregister_worker, Key}, State) when is_list(Key) ->
  {noreply, erase_token(State, Key)};

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%% Internal functions
store_state(State, Token, WorkerPid) ->
  #state{workers=Workers} = State,
  W1 = case dict:find(WorkerPid, Workers) of
         error ->
           dict:store(WorkerPid, [Token], Workers);
         {ok, Tokens} ->
           case lists:member(Token, Tokens) of
             true ->
               Workers;
             false ->
               dict:store(WorkerPid, [Token|Tokens], Workers)
           end
       end,
  State#state{workers=dict:store(Token, WorkerPid, W1)}.

erase_pid(State, Pid) ->
  case dict:find(Pid, State#state.workers) of
    error ->
      State;
    {ok, Tokens} ->
      W1 = lists:foldl(fun(T, Acc) ->
                          dict:erase(T, Acc) end, State#state.workers, Tokens),
      State#state{workers=dict:erase(Pid, W1)}
  end.

erase_token(State, Token) ->
  case dict:find(Token, State#state.workers) of
    error ->
      State;
    {ok, Pid} ->
      {ok, Tokens} = dict:find(Pid, State#state.workers),
      NewTokens = lists:delete(Token, Tokens),
      if
        length(NewTokens) == 0 ->
          State#state{workers=dict:erase(Pid, State#state.workers)};
        true ->
          State#state{workers=dict:store(Pid, lists:delete(Token, Tokens), State#state.workers)}
      end
  end.

lookup_token(Token, State) ->
  case dict:find(Token, State#state.workers) of
    error ->
      not_found;
    {ok, Value} when is_pid(Value) ->
      {ok, Value}
  end.

find_token([{_, _, Attrs, _}|T]) ->
  case proplists:get_value("token", Attrs) of
    undefined ->
      find_token(T);
    Token ->
      Token
  end.
