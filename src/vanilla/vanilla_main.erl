-module(vanilla_main).
-compile(export_all).
% NumberOfPings, TimeTaken

log(File, []) ->
    ok;
log(File, Results) ->
    file:write_file(File, lists:concat(Results), [append]).

test_iteration(_ServerPID, _ClientPID, _NumPings, 0, LogName, Results) ->
  log(LogName, lists:reverse(Results));
test_iteration(ServerPID, ClientPID, NumPings, Runs, LogName, Results) ->
  vanilla_client:start_test(ClientPID, ServerPID, NumPings),
  receive
    {done, Diff} ->
      ResultEntry = io_lib:format("vanilla_erlang, ~p, ~p~n", [NumPings, Diff]),
      ResultEntry1 = lists:flatten(ResultEntry),
      test_iteration(ServerPID, ClientPID, NumPings, (Runs - 1), LogName, [ResultEntry1|Results])
  end.

test_loop(ServerPID, ClientPID, Current, Max, Interval, Runs, LogName) when Current < Max ->
  test_iteration(ServerPID, ClientPID, Current + Interval, Runs, LogName, []),
  test_loop(ServerPID, ClientPID, (Current + Interval), Max, Interval, Runs, LogName);
test_loop(_, _, _, _, _, _, _) ->
  ok. %io:format("Done!~n").

start(Min, Max, Interval, Runs, LogName) ->
  {ok, ServerPID} = vanilla_server:start(),
  {ok, ClientPID} = vanilla_client:start(),
  test_loop(ServerPID, ClientPID, Min, Max, Interval, Runs, LogName).

