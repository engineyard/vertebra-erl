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

-module(vertebra).

-behaviour(application).

%% API
-export([start/0, stop/0]).

%% Application callbacks
-export([start/2, stop/1]).

%%====================================================================
%% API
%%====================================================================
start() ->
  application:start(?MODULE).

stop() ->
  application:stop(?MODULE).

%%====================================================================
%% Application callbacks
%%====================================================================
start(_Type, StartArgs) ->
  case vertebra_sup:start_link(StartArgs) of
    {ok, Pid} -> {ok, Pid};
    Error -> Error
  end.

stop(_State) ->
  ok.

%%====================================================================
%% Internal functions
%%====================================================================
