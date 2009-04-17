-module(vagent).
-behaviour(gen_server).

-export([start_link/0]).
-export([init/1,terminate/2,code_change/3]).
-export([handle_call/3,handle_cast/2,handle_info/2]).

-record(agent_state,{}).

start_link() ->
  supervisor:start_link(?MODULE,[]).

init(_Args) -> {ok, #agent_state{}}.

handle_call(_Request,_From,State) -> {reply,fail,State}.
handle_cast(_Request,State) -> {noreply,State}.
handle_info(_Info,State) -> {noreply,State}.

terminate(_Reason,_State) -> ok.

code_change(_OldVsn,State,_Extra) -> {ok,State}.
