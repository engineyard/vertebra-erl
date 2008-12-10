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

-module(herault_worker).

-author("ksmith@engineyard.com").

%% Standard TTL -- one hour in seconds
-define(STD_TTL, 3600).

-behaviour(gen_server).

%% API
-export([start_link/4]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-export([kickstart/1]).

-record(state,
	{owner,
	 from,
	 op,
	 token}).

start_link(ServerPid, From, Token, Op) ->
  gen_server:start_link(?MODULE, [ServerPid, From, Token, Op], []).

kickstart(Worker) ->
  gen_server:cast(Worker, start).

init([ServerPid, From, Token, Op]) ->
  {ok, #state{owner=ServerPid,
	      from=From,
	      op=Op,
	      token=Token}}.

handle_call(_Request, _From, State) ->
  Reply = ok,
  {reply, Reply, State}.

handle_cast(start, State) ->
  #state{op=Op} = State,
  {xmlelement, "op", Attrs, _} = Op,
  handle_request(proplists:get_value("type", Attrs), State);

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%% Internal functions
handle_request("/security/discover", State) ->
  {ok, Resources} = xml_util:convert(from, get_args(State)),
  {_, TTL, _} = erlang:now(),
  case listing_store:listing_query(lists:map(fun({resource, _Attr, ResName}) -> binary_to_list(ResName) end, Resources), TTL) of
    {ok, Jids} ->
      %% Ugly hack for now
      {ok, Result} = case string:str(State#state.from, "cavalcade") > 0 of
		       true ->
			 xml_util:convert(to, build_refs(Jids));
		       false ->
			 xml_util:convert(to, {list, [{"name", "jids"}], build_refs(Jids)})
		     end,
      gen_actor:send_result(State#state.owner, State#state.from, State#state.token, Result);
    _Error ->
      io:format("!! Error (~p: ~p): ~p~n", [?FILE, ?LINE, _Error]),
      gen_actor:send_error(State#state.owner, State#state.from, State#state.token, "Error processing query")
  end,
  gen_actor:end_result(State#state.owner, State#state.from, State#state.token),
  {stop, normal, State};

handle_request("/security/advertise", State) ->
  {ok, Args} = xml_util:convert(from, get_args(State)),
  {Resources, {TTL, Timestamp}, AdvertiserJid} = extract_args(Args),
  RegistrationJid = case AdvertiserJid of
		      undefined ->
			State#state.from;
		      _ ->
			[FromUser|_] = string:tokens(State#state.from, "/"),
			[AdvertiserJidUser|_] = string:tokens(AdvertiserJid, "/"),
			if
			  FromUser =:= AdvertiserJidUser ->
			    AdvertiserJid;
			  true ->
			    State#state.from
			end
		    end,
  case TTL of
    0 ->
      case remove_resources(RegistrationJid, Resources, Timestamp) of
	ok ->
	  ok;
	{error, Err} ->
	  gen_actor:send_error(State#state.owner, State#state.from, State#state.token, Err)
      end;
    _ ->
      case listing_store:add_listing(RegistrationJid, Resources, Timestamp) of
	ok ->
          {ok, TTLElement} = xml_util:convert(to, {i4, [{"name", "ttl"}], TTL}),
	  gen_actor:send_result(State#state.owner, State#state.from, State#state.token, TTLElement);
	_Error ->
	  gen_actor:send_result(State#state.owner, State#state.from, State#state.token, "Error recording resources")
      end,
      gen_actor:end_result(State#state.owner, State#state.from, State#state.token)
  end,
  {stop, normal, State};

handle_request("/security/authorize", State) ->
  {ok, SubEls} = xml_util:convert(from, get_args(State)),
  AuthCheck = case extract_auth_target(SubEls) of
 		not_found ->
 		  {string, [{"name", "response"}], <<"notauthorized">>};
 		Target ->
 		  case auth_store:has_access(convert_to_list(Target)) of
 		    true ->
 		      {string, [{"name", "response"}], <<"authorized">>};
 		    false ->
 		      {string, [{"name", "response"}], <<"notauthorized">>}
 		  end
 	      end,
  {ok, AuthResult} = xml_util:convert(to, AuthCheck),
  gen_actor:send_result(State#state.owner, State#state.from, State#state.token, AuthResult),
  gen_actor:end_result(State#state.owner, State#state.from, State#state.token),
  {stop, normal, State};

handle_request(Ignored, State) ->
  gen_actor:send_error(State#state.owner, State#state.from, State#state.token, lists:flatten(["Unknown op: ", Ignored])),
  gen_actor:end_result(State#state.owner, State#state.from, State#state.token),
  {stop, normal, State}.

%% Internal functions
remove_resources(RegistrationJid, Resources, TTL) ->
  case length(Resources) of
    0 ->
      case  listing_store:delete_listing(RegistrationJid) of
	ok ->
	  ok;
	_Error ->
          error_logger:error_msg("Delete error: ~p~n", [_Error]),
          {error, "Error deleting resources"}
      end;
    _ ->
      case listing_store:delete_resources(RegistrationJid, Resources, TTL) of
        ok ->
	  ok;
	_Error ->
          error_logger:error_msg("Delete error: ~p~n", [_Error]),
          {error, "Error deleting resources"}
      end
  end.

extract_args(Args) ->
  {_, Now, _} = erlang:now(),
  extract_args(Args, {[], {?STD_TTL, Now + ?STD_TTL}, undefined}).

extract_args([{list, [{"name", "resources"}], Resources}|T], {_, TTL, AdvertiserJid}) ->
  Names = lists:map(fun({resource, Attrs, Resource}) ->
			case proplists:get_value("name", Attrs) of
			  undefined ->
			    binary_to_list(Resource);
			  Value ->
			    Value
			end end, Resources),
  extract_args(T, {Names, TTL, AdvertiserJid});

extract_args([{resource, _, Resource}|T], {Resources, TTL, AdvertiserJid}) ->
  extract_args(T, {[binary_to_list(Resource)|Resources], TTL, AdvertiserJid});

extract_args([{integer, [{"name", "ttl"}], ClientTTL}|T], {R, _, AdvertiserJid}) ->
  {_, Now, _} = erlang:now(),
  FinalTTL = if
	       ClientTTL == 0 ->
		 ClientTTL;
	       true ->
		 Now + ClientTTL
	     end,
  extract_args(T, {R, {ClientTTL, FinalTTL}, AdvertiserJid});

extract_args([{string, [{"name", "advertiser"}], AdvertiserJid}|T], {R, TTL, _}) ->
  extract_args(T, {R, TTL, binary_to_list(AdvertiserJid)});

extract_args([], Accum) ->
  Accum.

get_args(State) ->
  #state{op=Op} = State,
  {xmlelement, "op", _Attrs, SubEls} = Op,
  SubEls.

get_single_arg(State) ->
  [H|_T] = get_args(State),
  H.

build_refs(Jids) ->
  lists:map(fun(Jid) ->
		{string, [], herault_util:jid_to_string(Jid)} end,
	    Jids).

extract_auth_target([{string, [{"name", "from"}], Value}|_]) when is_binary(Value) ->
  V = binary_to_list(Value),
  case string:str(V, "/") of
    0 ->
      Value;
    Pos ->
      list_to_binary(string:substr(V, 1, Pos - 1))
  end;
extract_auth_target([_H|T]) ->
  extract_auth_target(T);
extract_auth_target([]) ->
  not_found.

convert_to_list(Data) when is_list(Data) ->
  Data;
convert_to_list(Data) when is_binary(Data) ->
  binary_to_list(Data).
