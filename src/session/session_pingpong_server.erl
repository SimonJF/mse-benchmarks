-module(session_pingpong_server).
-behaviour(ssa_gen_server).
-compile(export_all).
-define(NODE, 'server@guadeloupe').

ssactor_init(_, _) ->
  no_state.

ssactor_join(_, State) -> {accept, State}.

ssactor_join(_, _, _, State) -> {accept, State}.

ssactor_conversation_established(_, _, _, ConvKey, State) ->
  {ok, State}.

ssactor_conversation_error(PN, RN, Error, State) ->
  error_logger:error_msg("Unable to set up session, PN: ~p, RN: ~p, Error: ~p~n",
                         [PN, RN, Error]),
  {ok, State}.

ssactor_conversation_ended(CID, Reason, State) ->
  %error_logger:info_msg("Session ~p ended for reason ~p~n",
  %                      [CID, Reason]),
  {ok, State}.

ssactor_handle_message(_, _, _, _, "ping", [], State, ConvKey) ->
  session_pingpong_client:pong(ConvKey),
  {ok, State}.

ssactor_become(_, _, _, _, State) -> {stop, unexpected_become, State}.
ssactor_subsession_complete(_, _, State, _) -> {stop, unexpected_become, State}.
ssactor_subsession_failed(_, _, State, _) -> {ok, State}.
ssactor_subsession_setup_failed(_, _, State, _) -> {ok, State}.

handle_call(_, _, State) -> {stop, unexpected_call, State}.
handle_cast(_, State) -> {stop, unexpected_cast, State}.
handle_info(_, State) -> {stop, unexpected_info, State}.
code_change(_, State, _) -> {ok, State}.
terminate(_, _) -> ok.

ping(ConvKey) ->
  conversation:send(ConvKey, ["B"], "ping", [], []).

start() ->
  rpc:call(?NODE, ssa_gen_server, start, [session_pingpong_server, [], []]).
