-module(vertebra_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
  supervisor:start_link({local,vertebra_sup},?MODULE,[]).

init(_Args) ->
  RestartBehavior = {one_for_one, 1, 60},
  Children = [],
  {ok,{RestartBehavior,Children}}.
