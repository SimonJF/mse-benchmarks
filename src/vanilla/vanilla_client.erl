-module(vanilla_client).
-behaviour(gen_server2).
-compile(export_all).
-define(NODE, 'client@guadeloupe').

init(_) ->
  {ok, undefined}.

handle_call(_, _, State) -> {stop, unexpected_call, State}.

handle_cast({start_test, ServerPID, NumPings, HarnessPID}, _)
  when is_integer(NumPings) andalso NumPings >= 0 ->
  StartTime = erlang:monotonic_time(),
  %io:format("Sending first ping~n"),
  vanilla_server:ping(ServerPID),
  {noreply, {ServerPID, NumPings - 1, HarnessPID, StartTime}};

handle_cast(pong, St={_, 0, HarnessPID, StartTime}) ->
  %io:format("Done! Sending back.~n"),
  EndTime = erlang:monotonic_time(),
  DiffNative = EndTime - StartTime,
  Diff = erlang:convert_time_unit(DiffNative, native, milli_seconds),
  HarnessPID ! {done, Diff},
  {noreply, St};
handle_cast(pong, {ServerPID, NumPings, HarnessPID, StartTime}) ->
  %io:format("Sending ping~n"),
  vanilla_server:ping(ServerPID),
  {noreply, {ServerPID, (NumPings - 1), HarnessPID, StartTime}}.

pong(ClientID) ->
  gen_server2:cast(ClientID, pong).

terminate(_, _) -> ok.
code_change(_, State, _) -> {ok, State}.

start_test(ClientPID, ServerPID, NumPings) ->
  gen_server2:cast(ClientPID, {start_test, ServerPID, NumPings, self()}).

start() ->
  rpc:call(?NODE, gen_server2, start, [vanilla_client, [], []]).
