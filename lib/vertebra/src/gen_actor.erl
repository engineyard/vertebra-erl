-module(gen_actor).

-behaviour(gen_server).

-define(DEFAULT_TTL, 3600).

%% API
-export([start_link/3, get_connection_info/1, send_fatal_error/4]).
-export([send_error/4, send_result/4, end_result/3]).
-export([add_resources/2, remove_resources/2, stop/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% gen_actor callbacks
-export([behaviour_info/1]).

-record(state,
        {xmpp,
         xmpp_config,
         cb_module,
         tracker_pid,
         advertiser}).

%%====================================================================
%% API
%%====================================================================
behaviour_info(callbacks) ->
  [{advertised_resources, 0},
   {handle_op, 5},
   {is_secure, 1}].

start_link(NameScope, Config, CallbackModule) when is_tuple(NameScope),
                                                   is_list(Config),
                                                   is_atom(CallbackModule) ->
  gen_server:start_link(NameScope, ?MODULE, [Config, CallbackModule], []).

get_connection_info(ServerPid) ->
  gen_server:call(ServerPid, get_xmpp_connection_info).

send_fatal_error(ServerPid, To, Token, Error) ->
  XMPP= get_connection_info(ServerPid),
  vertebra_xmpp:send_wait_set(XMPP, {}, To, op_builder:error_op("fatal", Error, Token)).

send_error(ServerPid, To, Token, Error) ->
  XMPP= get_connection_info(ServerPid),
  vertebra_xmpp:send_wait_set(XMPP, {}, To, ops_builder:error_op(Error, Token)).

send_result(ServerPid, To, Token, Result) ->
  XMPP= get_connection_info(ServerPid),
  vertebra_xmpp:send_wait_set(XMPP, {}, To, ops_builder:result_op(Result, Token)).

end_result(ServerPid, To, Token) ->
  XMPP= get_connection_info(ServerPid),
  vertebra_xmpp:send_wait_set(XMPP, {}, To, ops_builder:final_op(Token)).

add_resources(ServerPid, Resources) ->
  gen_server:call(ServerPid, {add_resources, Resources}).

remove_resources(ServerPid, Resources) ->
  gen_server:call(ServerPid, {remove_resources, Resources}).

stop(ServerPid) ->
  gen_server:cast(ServerPid, stop).

init([Config, CallbackModule]) ->
  {ok, Cn} = natter_connection:start_link(Config),
  natter_connection:register_default_exchange(Cn, self()),
  State = case proplists:get_value(herault, Config) of
            undefined ->
              #state{xmpp=Cn, cb_module=CallbackModule, xmpp_config=Config};
            _ ->
              {ok, P} = start_advertiser(Config, CallbackModule:advertised_resources()),
              #state{xmpp=Cn, cb_module=CallbackModule, xmpp_config=Config, advertiser=P}
          end,
  {ok, State}.

handle_call(get_xmpp_connection_info, _From, State) ->
  {reply, State#state.xmpp, State};

handle_call({remove_resources, Resources}, _From, State) ->
  gen_actor_advertiser:remove_resources(State#state.advertiser, Resources),
  {reply, ok, State};

handle_call({add_resources, Resources}, _From, State) ->
  gen_actor_advertiser:add_resources(State#state.advertiser, Resources),
  {reply, ok, State};

handle_call(_Msg, _From, State) ->
  {reply, ignored, State}.

handle_cast(stop, State) ->
  {stop, shutdown, State};

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info({packet, {xmlelement, "iq", Attrs, SubEls}=Stanza}=Info, State) ->
  case proplists:get_value("type", Attrs) of
    "result" ->
      io:format("Skipping bad starting packet: ~p~n", [Stanza]);
    _ ->
      case find_vertebra_element(SubEls) of
        undefined ->
          io:format("Dropping packet: ~p~n", [Info]);
        {xmlelement, _, OpAttrs, _} = Op ->
          From = proplists:get_value("from", Attrs),
          PacketId = proplists:get_value("id", Attrs),
          Token = proplists:get_value("token", OpAttrs),
          Me = self(),
          proc_lib:spawn(fun() -> dispatch(Me, State, From, PacketId, rotate_token(Token), Op) end)
      end
  end,
  {noreply, State};

handle_info(Info, State) ->
  io:format("Dropping packet: ~p~n", [Info]),
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%% Internal functions
rotate_token(Token) ->
  Parts = string:tokens(Token, ":"),
  case length(Parts) of
    1 ->
      lists:flatten([Parts, ":", uuid_server:generate_uuid()]);
    2 ->
      lists:flatten([lists:nth(2, Parts), ":", uuid_server:generate_uuid()])
  end.

verify_permissions(Config, Herault, Caller, Resources) ->
  R = vertebra_auth:verify(Config, Herault, Caller, Resources),
  io:format("R: ~p~n", [R]),
  case R of
    {ok, Result} ->
      Result;
    _ ->
      false
  end.

dispatch(ServerPid, ServerState, From, PacketId, Token, Op) ->
  #state{xmpp=Connection, xmpp_config=Config, cb_module=Mod} = ServerState,
  {xmlelement, OpName, _, _} = Op,
  CanContinue = case Mod:is_secure(OpName) of
                  true ->
                    verify_permissions(Config,
                                       proplists:get_value(herault, Config),
                                       From,
                                       [OpName]);
                  false ->
                    true
                end,
  if
    CanContinue =:= true ->
      vertebra_xmpp:confirm_op(Connection, {}, From, Op, Token, PacketId, true),
      run_callback(ServerPid, ServerState, From, Token, Op);
    true ->
      vertebra_xmpp:confirm_op(Connection, {}, From, Op, Token, PacketId, false)
  end.

run_callback(ServerPid, ServerState, From, Token, Op) ->
  {xmlelement, "op", Attrs, _} = Op,
  OpName = proplists:get_value("type", Attrs),
  Args = [OpName, ServerPid, From, Token, Op],
  try
    apply(ServerState#state.cb_module, handle_op, Args)
  catch
    error:undef ->
      Error = lists:flatten(["No handler for op: ", OpName]),
      vertebra_xmpp:send_wait_set(ServerState#state.xmpp, {}, From, op_builder:error_op("fatal", Error, Token)),
      vertebra_xmpp:send_wait_set(ServerState#state.xmpp, {}, From, ops_builder:final_op(Token))
  end.

find_vertebra_element([{xmlelement, Name, _, _}=H|_T]) when Name =:= "op";
                                                            Name =:= "ack";
                                                            Name =:= "result";
                                                            Name =:= "final" ->
  H;
find_vertebra_element([_H|T]) ->
  find_vertebra_element(T);
find_vertebra_element([]) ->
  undefined.

start_advertiser(Config, {}) ->
  gen_actor_advertiser:start_link(Config, [], ?DEFAULT_TTL);
start_advertiser(Config, {Resources}) ->
  gen_actor_advertiser:start_link(Config, Resources, ?DEFAULT_TTL);
start_advertiser(Config, {TTL, Resources}) ->
  gen_actor_advertiser:start_link(Config, Resources, TTL).
