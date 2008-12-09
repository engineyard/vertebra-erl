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

-module(test_listing_store).

-author("ksmith@engineyard.com").

-include_lib("eunit/include/eunit.hrl").
-include("tests.hrl").

-define(verify_all_agents(Results),
        fun() ->
            ?assertEqual(2, length(Results)),
            ?assertEqual(true, lists:member(?AGENT_1_JID, Results)),
            ?assertEqual(true, lists:member(?AGENT_2_JID, Results))
        end()).


add_listing_test_() ->
  {_, TTL, _} = erlang:now(),
  test_util:new_bundle(?SETUP_ENV([{listing_store, init, []},
				   {auth_store, init, ["./example/herault.cfg"]}]),
                       ?TEARDOWN,
                       [?_assertMatch(ok, listing_store:add_listing(?AGENT_1_JID, ?AGENT_1_RESOURCES, TTL + 3600)),
                        ?_assertMatch(ok, listing_store:add_listing(?AGENT_1_JID, ?AGENT_RESOURCE, TTL + 3600)),
                        fun() ->
                            mnesia:delete_table(resource_listing),
			    ?assertMatch({error, _}, listing_store:add_listing(?AGENT_1_JID, ?AGENT_1_RESOURCES, TTL + 3600)) end]).

modify_listing_test_() ->
  {_, TTL, _} = erlang:now(),
  test_util:new_bundle(?SETUP_ENV([{listing_store, init, []},
				   {listing_store, add_listing, [?AGENT_1_JID, ?AGENT_1_RESOURCES, TTL + 3600]}]),
		       ?TEARDOWN,
		       [fun() ->
			    ?assertMatch(ok, listing_store:delete_resources(?AGENT_1_JID, ["/node", "/gem"], TTL + 3600)),
			    ?assertMatch({ok, [?AGENT_1_JID]}, listing_store:listing_query("/cluster/rd00", TTL)) end,
		       fun() ->
			  {ok, Results} = listing_store:listing_query(["/gem"], TTL),
			   ?assertEqual(0, length(Results)) end]).


query_test_() ->
  {_, TTL, _} = erlang:now(),
  Actions = [{listing_store, init, []},
	     {auth_store, init, ["./example/herault.cfg"]},
	     {listing_store, add_listing, [?AGENT_1_JID, ?AGENT_1_RESOURCES, TTL + 3600]},
             {listing_store, add_listing, [?AGENT_2_JID, ?AGENT_2_RESOURCES, TTL + 3600]}],
  test_util:new_bundle(?SETUP_ENV(Actions),
		       ?TEARDOWN,
                       [fun() ->
                            {ok, Results} = listing_store:listing_query(?AGENT_1_RESOURCES, TTL),
			    ?assertMatch(1, length(Results)) end,
                        fun() ->
                            {ok, []} = listing_store:listing_query(?AGENT_RESOURCE, TTL) end,
                        fun() ->
                            {ok, Results} = listing_store:listing_query(["/node"], TTL),
                            ?verify_all_agents(Results) end,
                        fun() ->
                            {ok, Results} = listing_store:listing_query([], TTL),
                            ?verify_all_agents(Results) end,
		       fun() ->
			   ok = listing_store:add_listing(?AGENT_2_JID, ?AGENT_1_RESOURCES, TTL + 3600),
			   ?assertMatch({ok, [?AGENT_2_JID]}, listing_store:listing_query(["/gem", "/cpu"], TTL)) end]).

bad_query_test_() ->
  {_, TTL, _} = erlang:now(),
  Actions = [{listing_store, init, []},
	     {listing_store, add_listing, [?AGENT_1_JID, ?AGENT_1_RESOURCES, TTL + 3600]}],
  test_util:new_bundle(?SETUP_ENV(Actions),
		       ?TEARDOWN,
		       [fun() ->
			    {ok, [?AGENT_1_JID]} = listing_store:listing_query(?AGENT_1_RESOURCES, TTL) end,
			fun() ->
			    {ok, []} = listing_store:listing_query(?AGENT_1_RESOURCES_BAD, TTL) end]).

expired_query_test_() ->
  {_, TTL, _} = erlang:now(),
  Actions = [{listing_store, init, []},
	     {auth_store, init, ["./example/herault.cfg"]},
	     {listing_store, add_listing, [?AGENT_1_JID, ?AGENT_1_RESOURCES, TTL - 3600]},
             {listing_store, add_listing, [?AGENT_2_JID, ?AGENT_2_RESOURCES, TTL + 3600]}],
  test_util:new_bundle(?SETUP_ENV(Actions),
                       ?TEARDOWN,
                       [fun() ->
                            {ok, []} = listing_store:listing_query(?AGENT_1_RESOURCES, TTL) end,
                        fun() ->
                            {ok, []} = listing_store:listing_query(?AGENT_RESOURCE, TTL) end,
                        fun() ->
                            {ok, [?AGENT_2_JID]} = listing_store:listing_query(["/node"], TTL) end,
                        fun() ->
                            {ok, [?AGENT_2_JID]} = listing_store:listing_query([], TTL) end]).
