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

-module(discovery).

-author("ksmith@engineyard.com").

-export([find/3]).

find(Config, HeraultJid, Resources) ->
  Login = convert_login(Config),
  Token = proplists:get_value(resource, Login),
  Inputs = vertebra_xmpp:build_resources(Resources),
  case vertebra_command:run(Login, "/security/discover", Inputs, Token, HeraultJid) of
    {ok, _From, Data} ->
      {ok, aggregate_results(Data)};
    Error ->
      Error
  end.

%% Internal function
aggregate_results(Results) ->
  aggregate_results(Results, []).

aggregate_results([{string, _, Value}|T], Accum) ->
  aggregate_results(T, [binary_to_list(Value)|Accum]);
aggregate_results([], Accum) ->
  Accum.

convert_login(Config) ->
  Token = uuid_server:generate_uuid(),
  [{resource, Token} | lists:filter(fun({Name, _Value}) ->
                                        if
                                          Name =:= resource ->
                                            false;
                                          true ->
                                            true
                                        end end, Config)].
