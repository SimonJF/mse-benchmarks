#!/usr/bin/env bash
erl -sname server@guadeloupe -config config/vanilla_server -pa ../monitored-session-erlang/ebin/ ebin/
