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

-export([send_set/3, send_result/4, confirm_op/6, confirm_op/7, get_named_arg/2, get_token/1]).
-export([send_set/4, send_result/5]).
-export([build_resources/1]).


%% TODO: Phase out send_* functions which do not use a TrackInfo argument

send_set(XMPP, TrackInfo, To, Body) when is_tuple(TrackInfo) ->
  start_track(TrackInfo),
  case try send_set(XMPP, To, Body)
       catch
         Err -> throw(Err)
       after
         end_track(TrackInfo)
       end of
    {error, timeout} ->
      io:format("Timed out...Retrying~n"),
      send_set(XMPP, TrackInfo, To, Body);
    Value ->
      case is_error(Value) of
        false ->
          Value;
        {true, E} ->
          case is_retryable(E) of
            true ->
              io:format("Retrying~n"),
              send_set(XMPP, TrackInfo, To, Body);
            false ->
              io:format("Unrecoverable error: ~p~n", [Value]),
              Value
          end
      end
  end.

send_set(XMPP, To, Body) ->
  %random_wait(),
  xmpp:iq(XMPP, "set", To, xml:element_to_string(Body)).

send_result(XMPP, TrackInfo, To, PacketId, Body) when is_tuple(TrackInfo) ->
  start_track(TrackInfo),
  case try send_result(XMPP, To, PacketId, Body)
       catch
         Err -> throw(Err)
       after
         end_track(TrackInfo)
       end of
    {error, timeout} ->
      io:format("Retrying~n"),
      send_result(XMPP, TrackInfo, To, PacketId, Body);
    Value ->
      case is_error(Value) of
        false ->
          Value;
        {true, E} ->
          case is_retryable(E) of
            true ->
              io:format("Retrying~n"),
              send_set(XMPP, TrackInfo, To, Body);
            false ->
              io:format("Unrecoverable error: ~p~n", [Value]),
              Value
          end
      end
  end.

send_result(XMPP, To, PacketId, Body) when is_tuple(Body) ->
  %random_wait(),
  send_result(XMPP, To, PacketId, [Body]);

send_result(XMPP, To, PacketId, Body) when is_list(Body) ->
  %random_wait(),
  Packet = {xmlelement, "iq",
            [{"to", To},
             {"xml:lang", "en"},
             {"type", "result"},
             {"id", PacketId}],
            Body},
  xmpp:send(XMPP, xml:element_to_string(Packet)).

confirm_op(XMPP, TrackInfo, From, Op, Token, PacketId, IsAck) ->
  {xmlelement, Name, OpAttrs, SubEls} = Op,
  case Name =:= "ack" of
    true ->
      ok;
    false ->
      FinalOpAttrs = dict:store("token", Token, dict:from_list(OpAttrs)),
      send_result(XMPP, TrackInfo, From, PacketId, {xmlelement, Name, dict:to_list(FinalOpAttrs), SubEls}),
      case IsAck of
	true ->
	  send_set(XMPP, TrackInfo, From, ops_builder:ack_op(Token));
	false ->
	  send_set(XMPP, TrackInfo, From, ops_builder:nack_op(Token))
      end
  end.


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

get_token([{xmlelement, _, Attrs, _}|_T]) ->
  proplists:get_value("token", Attrs).

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

random_wait() ->
%%  {T1, T2, T3} = erlang:now(),
%%  random:seed(T1, T2, T3),
%%  Wait = random:uniform(15) * 1000,
  Wait = 5000,
  io:format("Sleeping ~p millis...", [Wait]),
  timer:sleep(Wait),
  io:format("Done~n").

start_track({TrackMod, TrackPid, Token}) ->
  TrackMod:register_request_worker(TrackPid, Token, self()).

end_track({TrackMod, TrackPid, Token}) ->
  TrackMod:unregister_request_worker(TrackPid, Token).

is_error({xmlstreamelement, _}) ->
  false;

is_error([H|T]) ->
  case H of
    {xmlelement, "error", _, _} ->
      {true, H};
    _ ->
      is_error(T)
  end;
is_error([]) ->
  false.

is_retryable({xmlelement, "error", Attrs, _}) ->
  case proplists:get_value("code", Attrs) of
    "404" ->
      true;
    _ ->
      false
  end.