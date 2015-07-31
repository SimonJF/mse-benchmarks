-module(session_pingpong_client).
-behaviour(ssa_gen_server).
-compile(export_all).
-record(client_state, {monitor_pid,
                       start_time,
                       pings_remaining,
                       harness_pid}).
-define(NODE, 'client@guadeloupe').


ssactor_init(_, MonitorPID) ->
  #client_state{monitor_pid=MonitorPID}.

ssactor_join(_, _, _, State) -> {accept, State}.

ssactor_conversation_established(_, _, _, ConvKey, State) ->
  StartTime = erlang:monotonic_time(),
  session_pingpong_server:ping(ConvKey),
  PingsRemaining = State#client_state.pings_remaining,
  NewState = State#client_state{start_time=StartTime,
                                pings_remaining=PingsRemaining-1},
  {ok, NewState}.

ssactor_conversation_error(PN, RN, Error, State) ->
  error_logger:error_msg("Unable to set up session, PN: ~p, RN: ~p, Error: ~p~n",
                         [PN, RN, Error]),
  {ok, State}.

ssactor_conversation_ended(CID, Reason, State) ->
  error_logger:info_msg("Session ~p ended for reason ~p~n",
                        [CID, Reason]),
  {ok, State}.

ssactor_handle_message(_, _, _, _, "pong", [], State, ConvKey) ->
  PingNum = State#client_state.pings_remaining,
  if PingNum > 0 ->
       session_pingpong_server:ping(ConvKey),
       NewState = State#client_state{pings_remaining=PingNum-1},
       {ok, NewState};
     PingNum == 0 ->
       StartTime = State#client_state.start_time,
       HarnessPID = State#client_state.harness_pid,
       EndTime = erlang:monotonic_time(),
       DiffNative = EndTime - StartTime,
       Diff = erlang:convert_time_unit(DiffNative, native, milli_seconds),
       HarnessPID ! {done, Diff},
       conversation:end_conversation(ConvKey, normal),
       {ok, State}
  end.

ssactor_become(_, _, _, _, State) -> {stop, unexpected_become, State}.
ssactor_subsession_complete(_, _, State, _) -> {stop, unexpected_become, State}.
ssactor_subsession_failed(_, _, State, _) -> {ok, State}.
ssactor_subsession_setup_failed(_, _, State, _) -> {ok, State}.

handle_call(_, _, State) -> {stop, unexpected_call, State}.
handle_cast({start_test, NumPings, HarnessPID}, State) ->
  MonitorPID = State#client_state.monitor_pid,
  NewState = State#client_state{pings_remaining=NumPings,
                                harness_pid=HarnessPID},
  conversation:start_conversation(MonitorPID, "PingPong", "A"),
  {noreply, NewState}.
handle_info(_, State) -> {stop, unexpected_info, State}.
code_change(_, State, _) -> {ok, State}.
terminate(_, _) -> ok.

pong(ConvKey) ->
  conversation:send(ConvKey, ["A"], "pong", [], []).

start_test(ClientPID, NumPings) ->
  gen_server2:cast(ClientPID, {start_test, NumPings, self()}).


start() ->
  rpc:call(?NODE, ssa_gen_server, start, [session_pingpong_client, [], []]).
