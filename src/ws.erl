-module(ws).
-export([start/0, stop/0, rebuild/0, d/1, d/0, log/2]).
-include("ws_recs.hrl").
-define(SERVER, {127,0,0,1}).
-define(PORT, 8088).

start() ->
	{S1, S2, S3} = now(),
	random:seed(S1, S2, S3),
	{ok, WD} = file:get_cwd(),
	yaws:start_embedded(io_lib:format("~s/site/", [WD]),
		[
			{id, "wsor"},
			{servername, "wsor"},
			{listen, ?SERVER},
			{port, ?PORT}
		],
		[
			{cache_refresh_secs, 0}
		],
		"wsor"
	),
	inets:start(),
	ssl:start(),
	kbk_lib:connect(),
	web_ajax_server:start(),
	ws_server:start(),
	ws_reminder:start(),
	ok.

log(S, Action) ->
	kbk_lib:add(S#pstate.logid, Action).

d() -> d("BREAKPOINT REACHED").
d(A) ->
	io:format("DEBUG: ~p~n", [A]), A.

rebuild() ->
	case make:all([load]) of
		up_to_date ->
			web_ajax_server:code_change(),
			ws_server ! code_change,
			ws_reminder ! code_change,
			{S1, S2, S3} = now(),
			random:seed(S1, S2, S3),
			ok;
		Else ->
			Else
	end.

stop() ->
	yaws:stop().
