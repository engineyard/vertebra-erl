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

%% TODO: Implement this for Natter

-module(gen_xmpp_client).

-export([start_link/3, cast/2]).
-export([behaviour_info/1]).

behaviour_info(callbacks) ->
  [{xmpp_init, 1},
   {recv_cast, 2},
   {presence, 6},
   {message, 8},
   {iq, 8}].

start_link(_Mod, _Args, _Options) ->
  {error, not_implemented}.

cast(_Pid, _Message) ->
  {error, not_implemented}.
