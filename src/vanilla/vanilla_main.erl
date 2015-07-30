-module(vanilla_main).
-compile(export_all).

test_iteration(_ServerPID, _ClientPID, NumPings, 0) ->
  io:format("Finished case with ~p pings~n", [NumPings]);
test_iteration(ServerPID, ClientPID, NumPings, Runs) ->
  io:format("Starting run ~p of case with ~p pings~n", [Runs, NumPings]),
  vanilla_client:start_test(ClientPID, ServerPID, NumPings),
  receive
    {done, StartTime, EndTime} ->
      io:format("Done, StartTime: ~p, EndTime: ~p, Diff: ~p~n",
                [StartTime, EndTime, (EndTime - StartTime)])
  end,
  test_iteration(ServerPID, ClientPID, NumPings, (Runs - 1)).

test_loop(ServerPID, ClientPID, Current, Max, Interval, Runs) when Current < Max ->
  test_iteration(ServerPID, ClientPID, Current + Interval, Runs),
  test_loop(ServerPID, ClientPID, (Current + Interval), Max, Interval, Runs);
test_loop(_, _, _, _, _, _) ->
  io:format("Done!~n").

start(Min, Max, Interval, Runs) ->
  {ok, ServerPID} = vanilla_server:start(),
  {ok, ClientPID} = vanilla_client:start(),
  test_loop(ServerPID, ClientPID, Min, Max, Interval, Runs).

