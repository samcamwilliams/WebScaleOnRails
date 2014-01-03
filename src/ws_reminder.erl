-module(ws_reminder).
-export([start/0, server/1]).
-record(state, {
	time = 0,
	actions = []
}).

start() ->
	register(ws_reminder, spawn(ws_reminder, server, [#state{ actions = actions() }])).

server(S) ->
	lists:foreach(
		fun({XT, XFun}) when (S#state.time rem XT) == 0 -> XFun();
		   (_) -> do_nothing
		end,
		S#state.actions
	),
	receive
		{add, XT, XFun} -> server(S#state { actions = S#state.actions ++ [{XT, XFun}] });
		code_change -> ws_reminder:server(S);
		stop -> ok
	after 1000 -> server(S#state { time = S#state.time + 1 })
	end.

actions() ->
	[
		{300, fun() -> ws_server ! refresh end},
		{1800, fun() -> ws_server ! backup end}
	].
