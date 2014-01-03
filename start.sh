#!/bin/bash
rm  -f ebin/*
erl -sname head-WS -setcookie kobokosrv -pa /usr/local/lib/yaws/ebin/ -s make all load -s ws start
#until erl -sname head-OP -setcookie kobokosrv -pa /usr/local/lib/yaws/ebin/ -s make all load -s op start; do
#	echo "optimise.me crashed with exit code $?.  Respawning... " >&2
#	rm  -f ebin/*
#	sleep 1
#done
