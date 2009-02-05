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

-module(entrepot_srv).

-author("stesla@engineyard.com").

-behaviour(gen_actor).

-include("models.hrl").

%% gen_actor callbacks
-export([start_link/1, advertised_resources/0, is_secure/1, handle_op/5]).

%% callback for entrepot_store
-export([result_callback/4]).

-define(TIMEOUT, 3600).

%%====================================================================
%% gen_actor callbacks
%%====================================================================
start_link(ConfigFile) ->
  Config = load_config(ConfigFile),
  gen_actor:start_link({local, ?MODULE}, Config, ?MODULE).

advertised_resources() ->
  Resources = key_resources(),
  {?TIMEOUT, ["/entrepot" | Resources]}.

is_secure(_) ->
  true.

handle_op("/entrepot/store", ServerPid, From, Token, Op) ->
  {xmlelement, _Name, _Attrs, SubEls} = Op,
  {ok, Key0} = xml_util:convert(from, entrepot_util:get_element("key", SubEls)),
  {ok, Value} = xml_util:convert(from, entrepot_util:get_element("value", SubEls)),
  Key = decode_key(Key0),
  {atomic, ok} = entrepot_store:store(Key, Value),
  advertise(ServerPid, Key),
  case result_callback(#item{key=Key, value=Value}, ServerPid, From, Token) of
    {error, {abort, _}} ->
      ok;
    {ok, UpdatedToken, _Reply} ->
      gen_actor:end_result(ServerPid, From, UpdatedToken)
  end;

handle_op("/entrepot/delete", ServerPid, From, Token, Op) ->
  {xmlelement, _Name, _Attrs, SubEls} = Op,
  {ok, [Key0]} = xml_util:convert(from, SubEls),
  Key = decode_key(Key0),
  case entrepot_store:delete(Key, {?MODULE, result_callback,
                                   [ServerPid, From, Token]}) of
    {atomic, ok} ->
      unadvertise(ServerPid, Key),
      gen_actor:end_result(ServerPid, From, Token);
    _ ->
      ok
  end;

handle_op("/entrepot/fetch", ServerPid, From, Token, Op) ->
  {xmlelement, _Name, _Attrs, SubEls} = Op,
  {ok, [Key0]} = xml_util:convert(from, SubEls),
  Key = decode_key(Key0),
  case entrepot_store:fetch(Key, {?MODULE, result_callback,
                                  [ServerPid, From, Token]}) of
    {atomic, ok} ->
      gen_actor:end_result(ServerPid, From, Token);
    _ ->
      ok
  end.

%%--------------------------------------------------------------------
%% Internal functions
%%--------------------------------------------------------------------

advertise(ServerPid, Key) ->
  Resources = key_to_list(Key),
  gen_actor:add_resources(ServerPid, Resources).

decode_key({struct, _Attrs, Children}) ->
  List = lists:map(fun({resource, Attrs, Resource}) ->
                       Name = proplists:get_value("name", Attrs),
                       {list_to_binary(Name), Resource}
                   end, Children),
  sets:from_list(List).

encode_key(Key) ->
  List = sets:to_list(Key),
  Resources = lists:map(fun({Name, Resource}) ->
                            Attrs = [{"name", binary_to_list(Name)}],
                            {resource, Attrs, Resource}
                        end, List),
  {struct, [{"name","key"}], Resources}.

key_to_list(Key) ->
  sets:fold(fun({_,Res}, Acc) -> [Res | Acc] end, [], Key).

load_config(ConfigFile) ->
  {ok, Config} = file:consult(ConfigFile),
  Config.

key_resources() ->
  {ok, Items} = entrepot_store:fetch(sets:new()),
  lists:foldl(fun(#item{key=Key}, Acc) ->
                  key_to_list(Key) ++ Acc
              end, [], Items).

make_result(#item{key=Key, value=Value}) ->
  {ok, Xml} =  xml_util:convert(to, [encode_key(Key), Value]),
  Xml.

result_callback(Result, Pid, From, Token) ->
  ResultXml = make_result(Result),
  gen_actor:send_result(Pid, From, Token, ResultXml).

unadvertise(ServerPid, Key) ->
  Resources = key_to_list(Key),
  gen_actor:remove_resources(ServerPid, Resources).
