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

-module(test_herault_worker).

-author("ksmith@engineyard.com").

-include_lib("eunit/include/eunit.hrl").

-include("tests.hrl").

-define(TOKEN, "foo:bar").
-define(IQID, "42").

-export([start/0, teardown/1]).

-define(RESOURCES, [{xmlelement,"res",[],
		     [{xmlcdata,<<"/cluster/rd00">>}]},
		    {xmlelement,"res",[],[{xmlcdata,<<"/slice/0">>}]},
		    {xmlelement,"res",[],[{xmlcdata,<<"/mock">>}]}]).

-define(ADVERTISE_OP, {xmlelement,"op",
		      [{"token", ?TOKEN},
		       {"type","/security/advertise"},
		       {"xmlns","http://xmlschema.engineyard.com/agent/api"}],
		       [{xmlelement, "list",
			 [{"name", "resources"}],
			 ?RESOURCES}]}).

-define(UNADVERTISE_OP, {xmlelement,"op",
			  [{"token", ?TOKEN},
			   {"type","/security/advertise"},
			   {"xmlns","http://xmlschema.engineyard.com/agent/api"}],
			  [{xmlelement, "list",
			    [{"name", "resources"}],
			 [{xmlelement, "res", [], [{xmlcdata, <<"/mock">>}]}]},
			  {xmlelement, "i4", [{"name", "ttl"}], [{xmlcdata, <<"0">>}]}]}).

-define(VERIFY_UNADVERTISE_OP,
	{xmlelement,"op",
		       [{"token","ac425bb1dabf2584bbc51774389aabb4"},
			{"type","/security/discover"},
			{"xmlns","http://xmlschema.engineyard.com/agent/api"}],
		       [{xmlelement, "res", [], [{xmlcdata, <<"/mock">>}]}]}).

-define(DISCOVERY_OP, {xmlelement,"op",
		       [{"token","ac425bb1dabf2584bbc51774389aabb4"},
			{"type","/security/discover"},
			{"xmlns","http://xmlschema.engineyard.com/agent/api"}],
		       ?RESOURCES}).

-define(assertAttr(Attrs, Key, Value),
        ?assertEqual(Value, proplists:get_value(Key, Attrs))).

-define(assertOpResult(OpResult, OpType),
        fun() ->
            {xmlelement, "iq", IqAttrs, IqElts} = OpResult,
            ?assertAttr(IqAttrs, "type", "result"),
            ?assertAttr(IqAttrs, "id", ?IQID),
            %% The protocol does not require that we have any children
            %% of the IQ sub-element, so we won't test for them.
            [{xmlelement, "op", OpAttrs, _OpElts} | _] = IqElts,
            ?assertAttr(OpAttrs, "type", OpType),
            ?assertAttr(OpAttrs, "token", ?TOKEN)
        end()).

start() ->
  {ok, Pid} = fake_xmpp:start_link(),
  herault_srv:start_link(Pid, []).

teardown(_Token) ->
  test_util:teardown(_Token),
  exit(whereis(herault_srv), shutdown),
  fake_xmpp:stop().

advertise_op_test_() ->
  test_util:new_bundle(?SETUP_ENV([{?MODULE, start, []}]),
		       {test_herault_worker, teardown},
		       [fun() ->
			    {ok, Results} = do_work(?ADVERTISE_OP, [{return_results, true}]),
			    [OpResult] = Results,
			    ?assertOpResult(OpResult, "/security/advertise")
			end]).

unadvertise_op_test_() ->
  test_util:new_bundle(?SETUP_ENV([{?MODULE, start, []}]),
		       {test_herault_worker, teardown},
		       [fun() ->
                    % TODO: We really should be looking at the return values,
                    % but we can't seem to get fake_xmpp to cooperate. So for
                    % now we're just going to assume that if it says it's okay,
                    % it is.
                    {ok, _} = do_work(?ADVERTISE_OP, [{return_results, true}]),
                    {ok, _} = do_work(?UNADVERTISE_OP, [{return_results, true}]),
                    {ok, _} = do_work(?VERIFY_UNADVERTISE_OP, [{return_results, true}])
			    end]).

discovery_op_no_results_test_() ->
  test_util:new_bundle(?SETUP_ENV([{?MODULE, start, []}]),
		       {test_herault_worker, teardown},
		       [fun() ->
                    {ok, Results} = do_work(?DISCOVERY_OP, [{return_results, true}]),
                    [OpResult] = Results,
                    ?assertOpResult(OpResult, "/security/discover")
                end]).


do_work(Op, Opts) ->
  ReturnResults = proplists:get_value(return_results, Opts, false),
  From = "test1@localhost/agent",
  Token = ?TOKEN,
  PacketId = ?IQID,
  {ok, Worker} = herault_worker:start_link(From, PacketId, Token, Op),
  herault_worker:kickstart(Worker),
  Results = receive
              {'EXIT', Worker, _} ->
                case ReturnResults of
                  true ->
                    fake_xmpp:extract();
                  false ->
                    ok
                end
            end,
  Results.
