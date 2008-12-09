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

-module(gen_actor).

-author("ksmith@engineyard.com").

-define(DEFAULT_TTL, 3600).

%% API
-export([behaviour_info/1, start_link/3, send_result/4, send_error/4, end_result/3]).
-export([add_resources/2, remove_resources/2, stop/1]).

%% Private gen_server <-> XMPP API
-export([handle_request/5, get_connection/1]).

%% gen_xmpp_client callbacks
-export([xmpp_init/1, recv_cast/2, presence/6, message/8, iq/8]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-behaviour(gen_xmpp_client).
-behaviour(gen_server).

-record(xmpp_state,
        {server_pid}).

-record(server_state,
        {advertiser,
         xmpp,
         xmpp_config,
         handlers,
         cb_module}).

behaviour_info(callbacks) ->
  [{advertised_resources, 0},
   {handle_op, 5},
   {is_secure, 1}].

start_link(NameScope, Config, CallbackModule) when is_tuple(NameScope),
                                                   is_list(Config),
                                                   is_atom(CallbackModule) ->
  {ok, XMPP} = gen_xmpp_client:start_link(?MODULE, Config, []),
  case gen_server:start_link(NameScope, ?MODULE, [XMPP, Config, CallbackModule], []) of
    {ok, Pid} ->
      {ok, Pid};
    Error ->
      xmpp:stop(XMPP),
      Error
  end.

handle_request(ActorPid, From, PacketId, Token, Op) ->
  gen_server:cast(ActorPid, {handle_request, From, PacketId, Token, Op}).

get_connection(ServerPid) ->
  gen_server:call(ServerPid, get_xmpp_connection).

send_error(ServerPid, To, Token, Error) ->
  XMPP = get_connection(ServerPid),
  vertebra_xmpp:send_set(XMPP, To, ops_builder:error_op(Error, Token)).

send_result(ServerPid, To, Token, Result) ->
  XMPP = get_connection(ServerPid),
  vertebra_xmpp:send_set(XMPP, To, ops_builder:result_op(Result, Token)).

end_result(ServerPid, To, Token) ->
  XMPP = get_connection(ServerPid),
  vertebra_xmpp:send_set(XMPP, To, ops_builder:final_op(Token)).

add_resources(ServerPid, Resources) ->
  gen_server:call(ServerPid, {add_resources, Resources}).

remove_resources(ServerPid, Resources) ->
  gen_server:call(ServerPid, {remove_resources, Resources}).

stop(ServerPid) ->
  gen_server:cast(ServerPid, stop).

%% Start gen_xmpp_client callbacks

xmpp_init(_Args) ->
  {ok, #xmpp_state{}}.

recv_cast({server_pid, ServerPid}, State) ->
  State#xmpp_state{server_pid=ServerPid};

recv_cast(_Msg, State) ->
  State.

%% Ignore presence packets
presence(_XMPP, _Type, _Who, _Attrs, _Elts, State) ->
  State.

%% Ignore chat message packets
message(_XMPP, _Type, _From, _Subject, _Body, _Attrs, _Elts, State) ->
  State.

iq(_XMPP, Type, From, _QueryNS, PacketId, _Attrs, Elts, State) ->
  case Type of
    "error" ->
      State;
    _ ->
      case get_token(Elts) of
        undefined ->
          State;
        %% Only process messages with a Vertebra token
        {Elt, Token} ->
          NewToken = rotate_token(Token),
          try
            gen_actor:handle_request(State#xmpp_state.server_pid, From, PacketId, NewToken, Elt)
          catch
            _: Reason ->
              #xmpp_state{server_pid=ServerPid} = State,
              gen_actor:send_error(ServerPid, From, NewToken, io_lib:format("~p", [Reason])),
              gen_actor:end_result(ServerPid, From, NewToken)
          end
      end
  end,
  State.

%% End gen_xmpp_client callbacks

%% Start gen_server callbacks

init([XMPP, Config, CallbackModule]) ->
  process_flag(trap_exit, true),
  gen_xmpp_client:cast(XMPP, {server_pid, self()}),
  Pid = case proplists:get_value(herault, Config) of
          undefined ->
            undefined;
          _ ->
            case CallbackModule:advertised_resources() of
              {} ->
                {ok, P} = gen_actor_advertiser:start_link(Config, [], ?DEFAULT_TTL),
                P;
              {AdvertiseInterval, Resources} ->
                {ok, P} = gen_actor_advertiser:start_link(Config, Resources, AdvertiseInterval),
                P
            end
        end,
  {ok, #server_state{xmpp=XMPP,
                     xmpp_config=Config,
                     cb_module=CallbackModule,
                     advertiser=Pid}}.

handle_call(get_xmpp_connection, _From, State) ->
  {reply, State#server_state.xmpp, State};

handle_call({add_resources, Resources}, _From, State) ->
  gen_actor_advertiser:add_resources(State#server_state.advertiser, Resources),
  {reply, ok, State};

handle_call({remove_resources, Resources}, _From, State) ->
  gen_actor_advertiser:remove_resources(State#server_state.advertiser, Resources),
  {reply, ok, State};

handle_call(_Msg, _From, State) ->
  {reply, ignored, State}.

handle_cast(stop, State) ->
  {stop, normal, State};

handle_cast({handle_request, From, PacketId, Token, Op}, State) ->
  OpName = extract_op(Op),
  #server_state{cb_module=CallbackModule, xmpp_config=XMPPConfig, xmpp=XMPP} = State,
  ServerPid = self(),
  try
    case CallbackModule:is_secure(OpName) of
      true ->
        %% TODO: This needs to extract ALL resources from an op and auth with all of them.
        case verify_permissions(XMPPConfig,
                                proplists:get_value(herault, XMPPConfig),
                                From,
                                [OpName]) of
          true ->
            dispatch_op(CallbackModule, XMPP, From, Op, Token, PacketId, ServerPid);
          false ->
            vertebra_xmpp:confirm_op(XMPP, From, Op, Token, PacketId, false)
        end;
      false ->
        dispatch_op(CallbackModule, XMPP, From, Op, Token, PacketId, ServerPid)
    end
  catch
    error:undef ->
      vertebra_xmpp:confirm_op(XMPP, From, Op, Token, PacketId, false)
  end,
  {noreply, State};

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info({'DOWN', _Ref, _Type, DownPid, _Info}, State) ->
  NewState = if
               DownPid == State#server_state.xmpp ->
                 {ok, XMPP} = gen_xmpp_client:start_link(?MODULE, State#server_state.xmpp_config, []),
                 State#server_state{xmpp=XMPP};
               DownPid == State#server_state.advertiser ->
                 #server_state{cb_module=CallbackModule} = State,
                 {_, AdvertiseInterval} = CallbackModule:advertised_resources(),
                 {ok, Pid} = gen_actor_advertiser:start_link(State#server_state.xmpp_config, [], AdvertiseInterval),
                 State#server_state{advertiser=Pid};
               true ->
                 State
             end,
  {noreply, NewState};

handle_info(_Msg, State) ->
  {noreply, State}.

terminate(_Reason, State) ->
  %% Stop the advertiser
  case State#server_state.advertiser of
    undefined ->
      ok;
    Pid ->
      gen_actor_advertiser:stop(Pid)
  end,
  %% Stop XMPP connection
  xmpp:stop(State#server_state.xmpp),
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%% End gen_server callbacks

%% Internal functions
dispatch_op(CallbackModule, XMPP, From, Op, Token, PacketId, ServerPid) ->
  OpName = extract_op(Op),
  vertebra_xmpp:confirm_op(XMPP, From, Op, Token, PacketId, true),
  %% TODO: this needs to be linked and supervised and stuff.
  proc_lib:spawn(fun() -> dispatch(CallbackModule, handle_op, [OpName, ServerPid, From, Token, Op]) end).

rotate_token(Token) ->
  Parts = string:tokens(Token, ":"),
  case length(Parts) of
    1 ->
      lists:flatten([Parts, ":", uuid_server:generate_uuid()]);
    2 ->
      lists:flatten([lists:nth(2, Parts), ":", uuid_server:generate_uuid()])
  end.

get_token([{xmlelement, Name, Attrs, _}=H|_T]) when Name =:= "op";
                                                    Name =:= "ack";
                                                    Name =:= "result";
                                                    Name =:= "final" ->
  case proplists:get_value("token", Attrs) of
    undefined ->
      undefined;
    Token ->
      {H, Token}
  end;
get_token([_H|T]) ->
  get_token(T);
get_token([]) ->
  undefined.

extract_op(OpXML) ->
  {xmlelement, "op", Attrs, _} = OpXML,
  proplists:get_value("type", Attrs).

verify_permissions(Config, HeraultJid, RequestorJid, Resources) ->
  case authz:verify(Config, HeraultJid, RequestorJid, Resources) of
    {ok, Result} ->
      Result;
    _ ->
      false
  end.

dispatch(Module, Function, [OpName, ServerPid, From, Token, _Op]=Args) ->
  try
    apply(Module, Function, Args)
  catch
    error:undef ->
      gen_actor:send_error(ServerPid, From, Token, lists:flatten(["No handler found for op: ", OpName])),
      gen_actor:end_result(ServerPid, From, Token)
  end,
  ok.
