-module(vanilla_server).
-behaviour(gen_server2).
-compile(export_all).
-define(NODE, 'server@guadeloupe').

init(_Args) ->
  {ok, no_state}.

handle_call(_, _, State) -> {stop, unexpected_call, State}.

handle_cast({ping, ClientID}, State) ->
  %io:format("Sending pong~n"),
  vanilla_client:pong(ClientID),
  {noreply, State}.

terminate(_, _) -> ok.
code_change(_, State, _) -> {no_change, State}.

ping(ServerID) ->
  gen_server2:cast(ServerID, {ping, self()}).

start() ->
  rpc:call(?NODE, gen_server2, start, [vanilla_server, [], []]).
