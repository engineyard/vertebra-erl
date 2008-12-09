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

-module(cavalcade_xmpp).

-export([send_set/3, send_result/4, confirm_op/6, get_named_arg/2]).

send_set(XMPP, To, Body) ->
  xmpp:iq(XMPP, "set", To, xml:element_to_string(Body)).

send_result(XMPP, To, PacketId, Body) when is_tuple(Body) ->
  send_result(XMPP, To, PacketId, [Body]);

send_result(XMPP, To, PacketId, Body) when is_list(Body) ->
  Packet = {xmlelement, "iq",
            [{"to", To},
             {"xml:lang", "en"},
             {"type", "result"},
             {"id", PacketId}],
            Body},
  xmpp:send(XMPP, xml:element_to_string(Packet)).

confirm_op(XMPP, From, Op, Token, PacketId, IsAck) ->
  {xmlelement, Name, OpAttrs, SubEls} = Op,
  case Name =:= "ack" of
    true ->
      ok;
    false ->
      FinalOpAttrs = dict:store("token", Token, dict:from_list(OpAttrs)),
      send_result(XMPP, From, PacketId, {xmlelement, Name, dict:to_list(FinalOpAttrs), SubEls}),
      case IsAck of
        true ->
          send_set(XMPP, From, ops_builder:ack_op(Token));
        false ->
          send_set(XMPP, From, ops_builder:nack_op(Token))
      end
  end.

get_named_arg(Name, Op) ->
  {ok, Args} = xml_util:convert(from, get_args(Op)),
  find_arg(Name, Args).

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
