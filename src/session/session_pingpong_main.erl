-module(session_pingpong_main).
-behaviour(application).
-compile(export_all).
% NumberOfPings, TimeTaken

log(File, []) ->
    ok;
log(File, Results) ->
    file:write_file(File, lists:concat(Results), [append]).

test_iteration(_ServerPID, _ClientPID, NumPings, 0, LogName, Results) ->
  io:format("Done ~p~n", [NumPings]),
  log(LogName, lists:reverse(Results));
test_iteration(ServerPID, ClientPID, NumPings, Runs, LogName, Results) ->
  session_pingpong_client:start_test(ClientPID, NumPings),
  receive
    {done, Diff} ->
      ResultEntry = io_lib:format("session_erlang, ~p, ~p~n", [NumPings, Diff]),
      ResultEntry1 = lists:flatten(ResultEntry),
      test_iteration(ServerPID, ClientPID, NumPings, (Runs - 1), LogName, [ResultEntry1|Results])
  end.

test_loop(ServerPID, ClientPID, Current, Max, Interval, Runs, LogName) when Current < Max ->
  test_iteration(ServerPID, ClientPID, Current + Interval, Runs, LogName, []),
  test_loop(ServerPID, ClientPID, (Current + Interval), Max, Interval, Runs, LogName);
test_loop(_, _, _, _, _, _, _) ->
  ok. %io:format("Done!~n").

start() ->
  application:start(monitored_session_erlang),
  %error_logger:tty(false),
  conversation:initialise("scribble_specs", session_pingpong_conf:config()).

stop(_) -> ok.

start_test(Min, Max, Interval, Runs, LogName) ->
  {ok, ServerPID} = session_pingpong_server:start(),
  {ok, ClientPID} = session_pingpong_client:start(),
  test_loop(ServerPID, ClientPID, Min, Max, Interval, Runs, LogName).

