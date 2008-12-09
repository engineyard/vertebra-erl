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

-module(herault_sup).

-author("ksmith@engineyard.com").

-behaviour(supervisor).

%% API
-export([start_link/1]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

start_link(ConfigFile) ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, [ConfigFile]).

init([ConfigFile]) ->
  process_flag(trap_exit, true),
  Children = [{herault_srv,
               {herault_srv, start_link, [ConfigFile]},
               transient,
               5000,
               worker,
               [herault_srv]}],
  {ok,{{one_for_one,1,60}, Children}}.
