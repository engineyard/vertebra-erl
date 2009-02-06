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

-module(vertebra_util).

-export([jid_from_config/1, md5/1]).

-define(JID_ATTRS, [user, host, resource]).

jid_from_config(XMPPConfig) ->
  lists:foldl(fun(Attr, Acc) -> resolve_attr(Attr, XMPPConfig, Acc) end, "", ?JID_ATTRS).

md5(Text) when is_list(Text) ->
  Checksum = binary_to_list(erlang:md5(Text)),
  lists:flatten([io_lib:format("~.16b", [X]) || X <- Checksum]).

%% Internal functions
resolve_attr(_, _, error) ->
  error;
resolve_attr(user, XMPPConfig, _Acc) ->
  case proplists:get_value(user, XMPPConfig) of
    undefined ->
      error;
    Value ->
      Value
  end;
resolve_attr(host, XMPPConfig, Acc) ->
  case proplists:get_value(host, XMPPConfig) of
    undefined ->
      error;
    Value ->
      Acc ++ "@" ++ Value
  end;
resolve_attr(resource, XMPPConfig, Acc) ->
  case proplists:get_value(resource, XMPPConfig) of
    undefined ->
      Acc;
    Value ->
      Acc ++ "/" ++ Value
  end.
