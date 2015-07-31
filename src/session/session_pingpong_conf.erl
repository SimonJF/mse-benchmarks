-module(session_pingpong_conf).
-export([config/0]).

config() ->
  [{session_pingpong_client, [{"PingPong", ["A"]}]},
   {session_pingpong_server, [{"PingPong", ["B"]}]}].
