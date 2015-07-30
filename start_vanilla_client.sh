#!/usr/bin/env bash
erl -sname client@guadeloupe -config config/vanilla_client -pa ../monitored-session-erlang/ebin/ ebin/
