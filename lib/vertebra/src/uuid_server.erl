% Copyright 2008, Engine Yard, Inc.
%
% This file is part of Vertebra.
%
% Vertebra is free software: you can redistribute it and/or modify it under the
% terms of the GNU Lesser General Public License as published by the Free
% Software Foundation, either version 3 of the License, or (at your option) any
% later version.
%
% Vertebra is distributed in the hope that it will be useful, but WITHOUT ANY
% WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR
% A PARTICULAR PURPOSE.  See the GNU Lesser General Public License for more
% details.
%
% You should have received a copy of the GNU Lesser General Public License
% along with Vertebra.  If not, see <http://www.gnu.org/licenses/>.

-module(uuid_server).

-behaviour(gen_server).

%% API
-export([start_link/0, generate_uuid/0, cache_size/0, uuid_generator/1, uuid_generator/2, shutdown/0]).

-define(SERVER, ?MODULE).
-define(CACHE_SIZE, 100).
-define(REFILL_THRESHOLD, 10).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state,
        {cache,
         worker_running}).

generate_uuid() ->
  conditional_start(),
  gen_server:call({global, ?SERVER}, gen_uuid).

cache_size() ->
  conditional_start(),
  gen_server:call({global, ?SERVER}, cache_size).

shutdown() ->
  gen_server:call({global, ?SERVER}, shutdown).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link() ->
  gen_server:start_link({global, ?SERVER}, ?MODULE, [], []).

%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------
init([]) ->
  crypto:start(),
  {ok, #state{cache=[], worker_running=fill_cache(0)}}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call(shutdown, _From, State) ->
  {stop, normal, State};

handle_call(cache_size, _From, State) ->
  {reply, length(State#state.cache), State};

handle_call(gen_uuid, _From, State) ->
  case length(State#state.cache) == 0 of
    true ->
      {reply, gen(), State};
    false ->
      {Retval, NewState} = fetch_uuid(State),
      {reply, Retval, NewState}
  end;

handle_call(_Request, _From, State) ->
  Reply = ok,
  {reply, Reply, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast({update_cache, Entries}, State) ->
  {noreply, State#state{worker_running=false, cache=lists:append(State#state.cache, Entries)}};

handle_cast(_Msg, State) ->
  {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
  {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
  ok.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
fetch_uuid(State) ->
  [H|T] = State#state.cache,
  if
    State#state.worker_running == true ->
      {H, State#state{cache=T}};
    true ->
      {H, State#state{cache=T, worker_running=fill_cache(length(State#state.cache))}}
  end.

fill_cache(CacheSize) ->
  case ?CACHE_SIZE - CacheSize > ?REFILL_THRESHOLD of
    true ->
      F = fun() -> uuid_generator(round((?CACHE_SIZE - CacheSize) * 1.5)) end,
      spawn(F),
      spawn(F),
      true;
    false ->
      false
  end.

uuid_generator(Count) ->
  uuid_generator(Count, []).


uuid_generator(0, Accum) ->
  gen_server:cast(?SERVER, {update_cache, Accum});
uuid_generator(Count, Accum) ->
  uuid_generator(Count - 1, lists:append(Accum, [gen()])).

%%-------------------------------------------------
                                                %
                                                % gen() -> uid
                                                %
                                                % uid -> string (32 chars long)
                                                %
                                                % TODO: Add a node component to the uuid
                                                %       to guarantee uniqueness across hosts
%%-------------------------------------------------
gen() ->
                                                % Figure out how many bytes to randomly skip
  SkipSize = round_to_byte(crypto:rand_uniform(0, 32)),

                                                % Determine the number of bytes needed to generate this uuid
  PoolSize = round((96 + (SkipSize * 2)) / 8),
  Pool = crypto:rand_bytes(PoolSize),

                                                % Extract the first, middle, last chunks of the uuid from the pool
                                                % making sure to skip where needed
  <<F:32/integer,_:SkipSize,M:32/integer,_:SkipSize,L:32/integer>> = Pool,

                                                % Convert all the chunks and the current timestamp to a uuid
  lists:flatten(to_hex(F) ++ to_hex(M) ++ to_hex(L) ++ get_time()).

round_to_byte(N) ->
  if
    N < 9 ->
      8;
    N < 17 ->
      16;
    N < 25 ->
      24;
    true ->
      32
  end.

get_time() ->
  {_, _, T} = now(),
  to_hex_pad(T).

to_hex(N) ->
  lists:flatten(io_lib:format("~8.16.0b", [N])).

to_hex_pad(N) ->
  lists:flatten(io_lib:format("~8.16.0b", [N])).

conditional_start() ->
  case global:whereis_name(?SERVER) of
    undefined ->
      uuid_server:start_link();
    _ ->
      ok
  end.
