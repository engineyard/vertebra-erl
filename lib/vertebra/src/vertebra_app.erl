-module(vertebra_app).
-behaviour(application).

-export([start/2, stop/1]).
-export([test/0]).

start(_Type, _Args) ->
  vertebra_sup:start_link().

stop(_State) -> stopped.

test() ->
  {ok,_} = appmon:start(),
  ok = application:start(sasl),
  ok = application:start(mnesia),
  ok = application:start(vertebra),
  ok.
