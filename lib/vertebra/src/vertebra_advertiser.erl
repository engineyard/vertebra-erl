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

-module(vertebra_advertiser).

-define(DEFAULT_TTL, 3600).

-export([advertise/2, advertise/3, unadvertise/2, unadvertise/3]).

advertise(Config, Resources) ->
  advertise(Config, Resources, ?DEFAULT_TTL).

advertise(Config, Resources, TTL) ->
  execute_command("/security/advertise", Config, Resources, TTL).

unadvertise(Config, Resources) ->
  unadvertise(Config, Resources, ?DEFAULT_TTL).

unadvertise(Config, Resources, TTL) ->
  execute_command("/security/unadvertise", Config, Resources, TTL).

convert_login(Config) ->
  [{resource, "advertiser"} | lists:filter(fun({Name, _Value}) ->
					if
					  Name =:= resource ->
					    false;
					  true ->
					    true
					end end, Config)].

execute_command(Op, Config, Resources, TTL) ->
  Login = convert_login(Config),
  Token = uuid_server:generate_uuid(),
  Res = vertebra_xmpp:build_resources(Resources),
  HeraultJid = proplists:get_value(herault, Login),
  AdvertiserJid = lists:flatten([proplists:get_value(username, Login), "@",
                                 proplists:get_value(host, Login), "/",
                                 proplists:get_value(username, Login)]),
  Inputs = [{string, [{"name", "advertiser"}], list_to_binary(AdvertiserJid)},
	    {list, [{"name", "resources"}], Res},
	    {integer, [{"name", "ttl"}], TTL}],
  case vertebra_cmd:run(Login, Op, Inputs, Token, HeraultJid) of
    {ok, _, _} ->
      ok;
    Error ->
      Error
  end.
