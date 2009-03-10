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

-module(vertebra_xmpp).

-define(MAX_PACKET_ID, 1000000).
-define(ILLEGAL_REPLY_ERR, {xmlelement, "error", [{"code", "406"}], [{xmlcdata, <<"Packet type not acceptable reply">>}]}).
-define(SEND_TIMEOUT, 120000).

-export([confirm_op/7, get_named_arg/2, get_token/1]).
-export([send_set/4, send_wait_set/4, send_result/5]).
-export([build_resources/1]).

send_set(XMPP, TrackInfo, To, Body) when is_tuple(TrackInfo), is_tuple(Body) ->
  send_set(XMPP, TrackInfo, To, natter_parser:element_to_string(Body));

send_set(XMPP, TrackInfo, To, Body) when is_tuple(TrackInfo), is_list(Body) ->
  natter_connection:send_iq(XMPP, "set", new_packet_id(), To, Body).

send_wait_set(XMPP, TrackInfo, To, Body) when is_tuple(TrackInfo), is_tuple(Body) ->
  send_wait_set(XMPP, TrackInfo, To, natter_parser:element_to_string(Body));

send_wait_set(XMPP, TrackInfo, To, Body) when is_tuple(TrackInfo), is_list(Body) ->
  case natter_connection:send_wait_iq(XMPP, "set", new_packet_id(), To, Body, ?SEND_TIMEOUT) of
    {error, timeout} ->
      send_wait_set(XMPP, TrackInfo, To, Body);
    Value ->
      Value
  end.

send_result(XMPP, TrackInfo, To, PacketId, Body) when is_tuple(TrackInfo), is_tuple(Body) ->
  send_result(XMPP, TrackInfo, To, PacketId, natter_parser:element_to_string(Body));

send_result(XMPP, TrackInfo, PacketId, To, Body) when is_tuple(TrackInfo), is_list(Body) ->
  natter_connection:send_iq(XMPP, "result", PacketId, To, Body).

confirm_op(XMPP, TrackInfo, From, Op, Token, PacketId, IsAck) ->
  {xmlelement, Name, OpAttrs, SubEls} = Op,
  case Name =:= "ack" of
    true ->
      ok;
    false ->
      FinalOpAttrs = dict:store("token", Token, dict:from_list(OpAttrs)),
      send_result(XMPP, TrackInfo, PacketId, From, {xmlelement, Name, dict:to_list(FinalOpAttrs), SubEls}),
      case IsAck of
        true ->
          case send_wait_set(XMPP, TrackInfo, From, vertebra_protocol:ack(Token)) of
            {ok, Reply} ->
              {ok, get_token(Reply)};
            Error ->
              Error
          end;
        false ->
          send_wait_set(XMPP, TrackInfo, From, vertebra_protocol:nack(Token))
      end
  end.

get_named_arg(Name, Op) ->
  {ok, Args} = xml_util:convert(from, get_args(Op)),
  find_arg(Name, Args).

get_token({xmlelement, _, _, Subels}) ->
  get_token(Subels);

get_token([{xmlelement, Name, Attrs, _}|_T]) when Name =:= "op";
                                                  Name =:= "ack";
                                                  Name =:= "data";
                                                  Name =:= "final" ->
  case proplists:get_value("token", Attrs) of
    undefined ->
      undefined;
    Token ->
      Token
  end;
get_token([_H|T]) ->
  get_token(T);
get_token([]) ->
  undefined.

build_resources(Resources) ->
  build_resources(Resources, []).

%% Internal functions
get_args(Op) ->
  {xmlelement, "op", _Attrs, SubEls} = Op,
  SubEls.

find_arg(Name, [{_Type, Attrs, _Value}=H|T]) ->
  case proplists:get_value("name", Attrs) of
    Name ->
      H;
    _ ->
      find_arg(Name, T)
  end;
find_arg(_Name, []) ->
  not_found.

build_resources([H|T], Accum) when is_list(H) ->
  build_resources(T, [{resource, [{"name", H}], list_to_binary(H)}|Accum]);
build_resources([H|T], Accum) when is_binary(H) ->
  build_resources(T, [{resource, [{"name", binary_to_list(H)}], H}|Accum]);
build_resources([], Accum) ->
  lists:reverse(Accum).

new_packet_id() ->
  PrevId = erlang:get(vertebra_iq_id),
  Id = crypto:rand_uniform(1, ?MAX_PACKET_ID),
  if
    PrevId =:= Id ->
      new_packet_id();
    true ->
      erlang:put(vertebra_iq_id, Id),
      integer_to_list(Id)
  end.
