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
-module(gen_actor_advertiser).

-behaviour(gen_server).

-define(FUDGE_FACTOR, 300).

%% API
-export([start_link/3, add_resources/2, remove_resources/2, readvertise/1, stop/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state,
        {xmpp_config,
         resources,
         advertise_interval,
         advertisement_timer,
         is_advertising=false}).

start_link(Config, Resources, AdvertiseInterval) ->
  gen_server:start_link(?MODULE, [Config, Resources, AdvertiseInterval], []).

add_resources(AdvertiserPid, Resources) ->
  gen_server:cast(AdvertiserPid, {add, Resources}).

remove_resources(AdvertiserPid, Resources) ->
  gen_server:cast(AdvertiserPid, {remove, Resources}).

readvertise(AdvertiserPid) ->
  gen_server:cast(AdvertiserPid, readvertise).

stop(AdvertiserPid) ->
  gen_server:cast(AdvertiserPid, stop).

init([Config, [], AdvertiseInterval]) ->
  process_flag(trap_exit, true),
  {ok, #state{
     xmpp_config=Config,
     advertise_interval=AdvertiseInterval,
     advertisement_timer=set_timer(AdvertiseInterval)}};

init([Config, Resources, AdvertiseInterval]) ->
  process_flag(trap_exit, true),
  vertebra_advertiser:advertise(Config, Resources, AdvertiseInterval + ?FUDGE_FACTOR),
  {ok, #state{
     xmpp_config=Config,
     resources=Resources,
     advertise_interval=AdvertiseInterval,
     advertisement_timer=set_timer(AdvertiseInterval)}}.

handle_call(_Request, _From, State) ->
  Reply = ok,
  {reply, Reply, State}.

handle_cast(stop, State) ->
  vertebra_advertiser:advertise(State#state.xmpp_config, [], 0),
  {stop, normal, State};

handle_cast(readvertise, State) ->
  vertebra_advertiser:advertise(State#state.xmpp_config, [],
                                State#state.advertise_interval + ?FUDGE_FACTOR),
  {noreply, State};

handle_cast({add, Resources}, State) ->
  if
    State#state.advertisement_timer == undefined ->
      ok;
    true ->
      timer:cancel(State#state.advertisement_timer)
  end,
  vertebra_advertiser:advertise(State#state.xmpp_config,
                       Resources,
                       State#state.advertise_interval + ?FUDGE_FACTOR),
  {noreply, State#state{advertisement_timer=set_timer(State#state.advertise_interval)}};

handle_cast({remove, Resources}, State) ->
  if
    State#state.advertisement_timer == undefined ->
      ok;
    true ->
      timer:cancel(State#state.advertisement_timer)
  end,
  vertebra_advertiser:advertise(State#state.xmpp_config, Resources, 0),
  {noreply, State#state{advertisement_timer=set_timer(State#state.advertise_interval)}};

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, State) ->
  timer:cancel(State#state.advertisement_timer),
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%% Internal functions
set_timer(AdvertisementInterval) ->
  ServerPid = self(),
  {ok, TRef} = timer:apply_interval(AdvertisementInterval * 1000,
                                    ?MODULE,
                                    readvertise,
                                    [ServerPid]),
  TRef.
