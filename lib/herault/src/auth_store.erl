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

-module(auth_store).

-author("ksmith@engineyard.com").

-behaviour(gen_store).

-include("models.hrl").

-export([init/1, table_name/0, table_info/0]).
-export([add_entry/1, has_access/1, delete_entry/1]).

init(Config) ->
  gen_store:init(?MODULE),
  case proplists:get_value(authorized_jids, Config) of
    undefined ->
      ok;
    AuthJids ->
      lists:foreach(fun(Jid) -> add_entry(Jid) end, AuthJids)
  end.

table_name() ->
  hauth_entry.

table_info() ->
  [{table_attrs, record_info(fields, hauth_entry)}].

add_entry(Jid) when is_tuple(Jid) ->
  add_entry(herault_util:jid_to_string(Jid));

add_entry(Jid) ->
  case gen_store:w(new_entry(normalize_jid(Jid))) of
    {atomic, ok} ->
      ok;
    Error ->
      Error
  end.

has_access(Jid) ->
  NormalJid = normalize_jid(Jid),
  case gen_store:sq(?MODULE,
                    fun(E) -> E#hauth_entry.access end,
                    [fun(E) -> E#hauth_entry.jid =:= NormalJid end]) of
    {atomic, [true]} ->
      true;
    {atomic, []} ->
      false;
    Error ->
      Error
  end.

delete_entry(Jid) ->
  case gen_store:dk(?MODULE, normalize_jid(Jid)) of
    {atomic, ok} ->
      ok;
    Error ->
      Error
  end.

%% Internal functions
new_entry(Jid) ->
  #hauth_entry{jid=Jid, access=true}.

normalize_jid(Jid) ->
  case string:str(Jid, "/") of
    0 ->
      Jid;
    Value ->
      string:substr(Jid, 1, Value - 1)
  end.
