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

-module(herault_srv).

-behaviour(gen_actor).

-author("ksmith@engineyard.com").

-export([start_link/1]).

-export([advertised_resources/0, is_secure/1, handle_op/5]).

-define(TIMEOUT, 3600).

start_link(ConfigFile) ->
  Config = load_config(ConfigFile),
  ok = listing_store:init(),
  ok = auth_store:init(Config),
  gen_actor:start_link({local, ?MODULE}, Config, ?MODULE).

advertised_resources() ->
  {?TIMEOUT, []}.

is_secure(_) ->
  false.

handle_op(_OpName, ServerPid, From, Token, OpTag) ->
  {ok, Pid} = herault_worker:start_link(ServerPid, From, Token, OpTag),
  herault_worker:kickstart(Pid).

%% Private functions
load_config(ConfigFile) ->
  {ok, Config} = file:consult(ConfigFile),
  Config.
