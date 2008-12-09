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

-module(listing_store).

-author("ksmith@engineyard.com").

-behaviour(gen_store).

-compile([export_all]).

-export([init/0, table_info/0, table_name/0, add_listing/3, delete_listing/1, delete_resources/3, listing_query/2]).

-include_lib("stdlib/include/qlc.hrl").
-include("models.hrl").

table_name() ->
  resource_listing.

table_info() ->
  [{table_attrs, record_info(fields, resource_listing)}].

init() ->
  gen_store:init(?MODULE).

add_listing(Jid, Res, TTL) when length(Res) > 0 ->
  case mnesia:transaction(fun() ->
                              lists:foreach(fun(R) ->
                                                RL = case qlc:e(qlc:q([L || L <- mnesia:table(resource_listing),
                                                                            L#resource_listing.resource =:= R])) of
                                                       [] ->
                                                         new_listing(R);
                                                       [Results] ->
                                                         Results
                                                     end,
                                                #resource_listing{advertisers=Advertisers} = RL,
                                                UpdatedAdvertisers = dict:store(Jid, TTL, Advertisers),
                                                mnesia:write(RL#resource_listing{advertisers=UpdatedAdvertisers}) end, Res) end) of
    {atomic, _} ->
      ok;
    Error ->
      {error, Error}
  end;

add_listing(Jid, _Res, TTL) ->
  case mnesia:transaction(fun() ->
                              Listings = qlc:e(qlc:q([L || L <- mnesia:table(resource_listing),
                                                           dict:is_key(Jid, L#resource_listing.advertisers) == true])),
                              lists:foreach(fun(L) ->
                                                #resource_listing{advertisers=Advertisers} = L,
                                                UpdatedAdvertisers = dict:store(Jid, TTL, Advertisers),
                                                mnesia:write(L#resource_listing{advertisers=UpdatedAdvertisers}) end, Listings) end) of
    {atomic, _} ->
      ok;
    Error ->
      {error, Error}
  end.

delete_listing(Jid) ->
  case mnesia:transaction(fun() ->
                              RL = qlc:e(qlc:q([L || L <- mnesia:table(resource_listing),
                                                     dict:is_key(Jid, L#resource_listing.advertisers)])),
                              lists:foreach(fun(R) ->
                                                Updated = R#resource_listing{advertisers=dict:erase(Jid, R#resource_listing.advertisers)},
                                                mnesia:write(Updated) end, RL) end) of
    {atomic, _} ->
      ok;
    Error ->
      {error, Error}
  end.

delete_resources(Jid, Res, TTL) ->
  case mnesia:transaction(fun() ->
                              RL = qlc:e(qlc:q([L || L <- mnesia:table(resource_listing),
                                                     lists:member(L#resource_listing.resource, Res),
                                                     dict:is_key(Jid, L#resource_listing.advertisers)])),
                              lists:foreach(fun(Listing) ->
                                                #resource_listing{advertisers=Advertisers} = Listing,
                                                mnesia:write(Listing#resource_listing{advertisers=dict:erase(Jid, Advertisers)}) end,
                                            RL) end) of
    {atomic, _} ->
      add_listing(Jid, [], TTL);
    Error ->
      {error, Error}
  end.

listing_query(Resources, TTL) when is_list(Resources) ->
  Query = prepare_query(insure_proper_list(Resources)),
  {atomic, R} = mnesia:transaction(fun() ->
                                       lists:foldl(fun({Exact, Prefix}, Acc) ->
                                                       Results = qlc:e(qlc:q([L#resource_listing.advertisers || L <- mnesia:table(resource_listing),
                                                                                                                is_match(L#resource_listing.resource,
                                                                                                                         Exact,
                                                                                                                         Prefix) == true])),
                                                       if
                                                         length(Results) == 0 ->
                                                           [sets:from_list([])|Acc];
                                                         true ->
                                                           [sets:from_list(filter_by_ttl(Results, TTL))|Acc]
                                                       end end, [], Query) end),
  case R of
    [] ->
      {ok, []};
    _ ->
      {ok, sets:to_list(sets:intersection(R))}
  end.

%% Internal Functions
filter_by_ttl(Advertisers, TTL) ->
  lists:foldl(fun(A, Accum) ->
                  A1 = dict:filter(fun(_Jid, ExpireTTL) -> ExpireTTL >= TTL end, A),
                  case dict:size(A1) of
                    0 ->
                      Accum;
                    _ ->
                      lists:merge([dict:fetch_keys(A1), Accum])
                  end end, [], Advertisers).

insure_proper_list([]) ->
  [];

insure_proper_list([H|_]=Data) when is_integer(H) ->
  [Data];

insure_proper_list(Data) ->
  Data.

new_listing(Resource) ->
  #resource_listing{resource=Resource, advertisers=dict:new()}.

is_match(_Resource, [], []) ->
  true;

is_match(Resource, ExactTerms, PrefixTerm) ->
  case lists:member(Resource, ExactTerms) of
    true ->
      true;
    false ->
      is_prefix_match(Resource, PrefixTerm)
  end.

is_prefix_match(Resource, Term) ->
  case string:str(Resource, Term) == 1 of
    true ->
      true;
    false ->
      false
  end.

prepare_query([]) ->
  [{[], []}];

prepare_query(Resources) ->
  [{decompose_term(string:tokens(R, "/")), R} || R <- Resources].

decompose_term([]) ->
  [];

decompose_term(R) ->
  {_, [_|T]} = lists:foldl(fun(T, {Prev, Accum}) ->
                               NT = Prev ++ "/" ++ T,
                               if
                                 R =:= NT ->
                                   {Prev, Accum};
                                 true ->
                                   {NT, [NT|Accum]}
                               end end, {"", []}, R),

  lists:sort(T).
