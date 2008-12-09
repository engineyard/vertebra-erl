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

-module(cavalcade_app).
-behaviour(application).

%% Application callbacks
-export([start/0, start/2, stop/1]).

start() ->
  ok = application:start(mnesia),
  ok = application:start(vertebra_common),
  application:start(cavalcade).

start(_Type, _StartArgs) ->
  {ok, ConfigFile} = application:get_env(config_file),
  cavalcade_sup:start_link(ConfigFile).

stop(_Start) ->
  ok.
