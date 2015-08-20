-module(monitors).
-compile(export_all).
-include("monitor_records.hrl").

log(_, []) ->
    ok;
log(File, Results) ->
    file:write_file(File, lists:concat(Results), [append]).

% (FSM ID |-> MonitorGenState) -> int
get_state_count(FSMs) ->
  orddict:fold(fun(_, FSMSpec, A) ->
                   A + orddict:size(FSMSpec#monitor_gen_state.states) end,
               0, FSMs).

get_transition_count(FSMs) ->
  orddict:fold(fun(_, FSMSpec, A) ->
                   Transitions = FSMSpec#monitor_gen_state.transitions,
                   A + num_transitions(orddict:to_list(Transitions)) end, 0, FSMs).

num_transitions([]) -> 0;
num_transitions([{_, TransitionSet}|XS]) ->
  sets:size(TransitionSet) + num_transitions(XS).

test_iteration(ProtocolName, RS = {local_protocol, _, Role, _, _, _}) ->
  {ok, BaseMonitor} = monitor:create_monitor(RS),
  FSMs = BaseMonitor#outer_monitor_instance.monitors,
  StateCount = get_state_count(FSMs),
  TransitionCount = get_transition_count(FSMs),
  Results = test_iteration_inner(RS, 10000),
  ResultEntries = lists:map(fun(Time) ->
                              ResultEntry = io_lib:format("~p, ~p, ~p, ~p, ~p~n",
                                                          [ProtocolName, Role, StateCount,
                                                           TransitionCount, Time]),
                              lists:flatten(ResultEntry)
                            end, Results),
  log("monitor_results.log", ResultEntries),
  ok.

test_iteration_inner(_RS, 0) -> [];
test_iteration_inner(RS, N) ->
  BeforeTime = erlang:monotonic_time(),
  monitor:create_monitor(RS),
  AfterTime = erlang:monotonic_time(),
  Diff = AfterTime - BeforeTime,
  DiffNano = erlang:convert_time_unit(Diff, native, nano_seconds),
  [DiffNano|test_iteration_inner(RS, N-1)].


perform_test(ProtocolName, RoleSpecs) ->
  lists:foreach(fun({_, RoleSpec}) ->
                    test_iteration(ProtocolName, RoleSpec) end, RoleSpecs).


main() ->
  % ProtocolDict: Protocol |-> (Role |-> RoleSpec)
  ProtocolDict = protocol_loader:load_protocol_files("scribble_specs"),
  ProtocolList = orddict:to_list(ProtocolDict),
  lists:foreach(fun({ProtocolName, RoleDict}) ->
                    perform_test(ProtocolName, RoleDict) end, ProtocolList).



