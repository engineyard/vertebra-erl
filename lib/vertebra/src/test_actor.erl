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

-module(test_actor).

-author("ksmith@engineyard.com").

-behaviour(gen_actor).

%% gen_actor callbacks
-export([start_link/0, advertised_resources/0, is_secure/1, handle_op/5]).

start_link() ->
  Config = [{host, "localhost"},
            {username, "cavalcade"},
            {authentication, {password, "testing"}},
            {resource, "cavalcade"},
            {herault, "herault@localhost/herault"}],
  gen_actor:start_link({local, ?MODULE}, Config, ?MODULE).

advertised_resources() ->
  %% Starting set of resources
  {1, ["/foo/bar", "/gem"]}.

is_secure("/foo/baz") ->
  false;

is_secure(_) ->
  true.

handle_op("/foo/bar", ServerPid, From, Token, _Op) ->
  {ok, R} = xml_util:convert(to, {string, [{"name", "result"}], <<"hello, /foo/bar">>}),
  gen_actor:send_result(ServerPid, From, Token, R),
  gen_actor:end_result(ServerPid, From, Token),
  %% Dynamically advertising a resource
  gen_actor:add_resources(ServerPid, ["/foo/baz"]);

handle_op("/foo/baz", ServerPid, From, Token, _Op) ->
  {ok, R} = xml_util:convert(to, {string, [{"name", "result"}], <<"hello, /foo/baz">>}),
  gen_actor:send_result(ServerPid, From, Token, R),
  gen_actor:end_result(ServerPid, From, Token),
  %% Dynamically removing a resource
  gen_actor:remove_resources(ServerPid, ["/foo/baz"]).
