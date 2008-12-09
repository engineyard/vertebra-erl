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

-module(cavalcade_srv).

-author("ksmith@engineyard.com").

-behaviour(gen_server).
-behaviour(gen_xmpp_client).

%% gen_server callbacks
-export([handle_call/3, handle_cast/2, handle_info/2, code_change/3, terminate/2, init/1, readvertise/0]).

-define(SERVER, ?MODULE).
-define(RESOURCE, ["/workflow"]).

%% Advertise resources every 30 minutes
%%-define(READVERTISE_INTERVAL, 1800000).
-define(READVERTISE_INTERVAL, 30000).

%% gen_xmpp_client callbacks
-export([xmpp_init/1, presence/6, message/8, iq/8, recv_cast/2]).

-export([start_link/1, send_result/3, send_set/2, confirm_op/5, find_worker/1, store_worker/2, get_config/0]).
-export([test/0]).

-record(state,
        {config,
         xmpp,
         advertiser,
         workers=dict:new()}).

test() ->
  mnesia:start(),
  cavalcade_srv:start_link("/etc/vertebra/cavalcade.cfg").

start_link(ConfigFile) ->
  Config = load_config(ConfigFile),
  {ok, XMPP} = gen_xmpp_client:start_link(?MODULE, Config, []),
  gen_server:start_link({local, ?SERVER}, ?MODULE, [XMPP, Config], []).

xmpp_init(_Args) ->
  {ok, []}.

recv_cast(_Msg, State) ->
  State.

confirm_op(From, Op, Token, PacketId, IsAck) ->
  {ok, XMPP} = get_connection(),
  cavalcade_xmpp:confirm_op(XMPP, From, Op, Token, PacketId, IsAck).

send_result(To, PacketId, Body) when is_tuple(Body) ->
  send_result(To, PacketId, [Body]);

send_result(To, PacketId, Body) when is_list(Body) ->
  {ok, XMPP} = get_connection(),
  cavalcade_xmpp:send_result(XMPP, To, PacketId, Body).

send_set(To, Body) when is_tuple(Body) ->
  {ok, XMPP} = get_connection(),
  cavalcade_xmpp:send_set(XMPP, To, Body).

find_worker(Token) ->
  gen_server:call(?SERVER, {find_worker, Token}).

store_worker(Token, WorkerPid) ->
  gen_server:call(?SERVER, {store_worker, Token, WorkerPid}).

get_connection() ->
  gen_server:call(?SERVER, get_connection).

get_config() ->
  gen_server:call(?SERVER, get_config).

readvertise() ->
  Config = cavalcade_srv:get_config(),
  spawn(fun() ->
            advertiser:advertise(Config, ?RESOURCE) end).

%% Ignore presence packets
presence(_XMPP, _Type, _Who, _Attrs, _Elts, State) ->
  State.

%% Ignore chat message packets
message(_XMPP, _Type, _From, _Subject, _Body, _Attrs, _Elts, State) ->
  State.

iq(_XMPP, _Type, From, _QueryNS, PacketId, _Attrs, Elts, State) ->
  [Elt|_T] = Elts,
  case get_token(Elt) of
    undefined ->
      State;
    Token ->
      NewToken = rotate_token(Token),
      {ok, Worker} = cavalcade_worker:start_link(From, PacketId, NewToken, Elt),
      cavalcade_worker:kickstart(Worker),
      State
  end.

                                                % gen_server callback
init([XMPP, Config]) ->
  process_flag(trap_exit, true),
  ok = workflow_store:init(),
  erlang:monitor(process, XMPP),
  {ok, TRef} = timer:apply_interval(?READVERTISE_INTERVAL, ?MODULE, readvertise, []),
  advertiser:advertise(Config, ?RESOURCE),
  {ok, #state{xmpp=XMPP, config=Config, advertiser=TRef}}.

handle_call(get_config, _From, State) ->
  {reply, State#state.config, State};

handle_call(get_connection, _From, State) ->
  {reply, {ok, State#state.xmpp}, State};

handle_call({find_worker, Token}, _From, State) ->
  Result = case dict:find(Token, State#state.workers) of
             error ->
               not_found;
             WorkerPid ->
               WorkerPid
           end,
  {reply, Result, State};

handle_call({store_worker, Token, WorkerPid}, _From, State) when is_pid(WorkerPid) ->
  #state{workers=Workers} = State,
  erlang:monitor(process, WorkerPid),
  {reply, ok, State#state{workers=dict:store(Token, WorkerPid, Workers)}};

handle_call(_Msg, _To, State) ->
  {reply, ignored, State}.

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info({'DOWN', _Ref, _Type, WorkerPid, _Info}, State) ->
  #state{workers=Workers} = State,
  UpdatedWorkers = dict:filter(fun(_Key, Value) ->
                                   case Value =:= WorkerPid of
                                     true ->
                                       false;
                                     false ->
                                       true
                                   end end, Workers),

  %% If no worker is found, then our XMPP connection
  %% has probably gone away, so let's restart
  case dict:size(UpdatedWorkers) == dict:size(Workers) of
    true ->
      {stop, normal, State};
    false ->
      {noreply, State#state{workers=UpdatedWorkers}}
  end;

handle_info(_Msg, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%% Internal Functions
load_config(ConfigFileName) ->
  {ok, Config} = file:consult(ConfigFileName),
  Config.

rotate_token(Token) ->
  Parts = string:tokens(Token, ":"),
  case length(Parts) of
    1 ->
      lists:flatten([Parts, ":", uuid_server:generate_uuid()]);
    2 ->
      lists:flatten([lists:nth(2, Parts), ":", uuid_server:generate_uuid()])
  end.

get_token({xmlelement, _, Attrs, _}) ->
  proplists:get_value("token", Attrs).
