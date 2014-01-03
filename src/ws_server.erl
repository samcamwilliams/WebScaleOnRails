-module(ws_server).
-export([start/0, server/1]).
-export([get/0]).
-include("ws_recs.hrl").
-record(state, {
	refreshing = false,
	items = []
}).

start() ->
	register(ws_server, spawn(ws_server, server, [#state{}])).

get() ->
	ws_server ! {get, self()},
	receive
		{items, Is} -> Is
	end.

server(S) ->
	receive
		refresh ->
			spawn(
				fun() ->
					ws_server ! {refresh, order(refresh(S#state.items, ws_scrape:get_latest())) }
				end
			),
			server(S#state { refreshing = true });
		{refresh, Data} ->
			server(
				S#state { refreshing = false, items = Data }
			);
		{up, ID, IP} when not S#state.refreshing ->
			server(S#state { items = order(up(S#state.items, ID, IP)) });
		{get, PID} ->
			PID ! {items, S#state.items},
			server(S);
		code_change -> ws_server:server(S);
		stop -> ok
	end.

order(Is) ->
	lists:reverse(
		lists:sort(
			fun(A, B) ->
				length(A#item.votes) =< length(B#item.votes)
			end,
			Is
		)
	).

up(Is, ID, IP) ->
	case lists:keyfind(ID, 2, Is) of
		false -> Is;
		X -> lists:keyreplace(ID, 2, Is, X#item { votes = X#item.votes ++ [IP] })
	end.

refresh(Current, New) ->
	lists:filter(
		fun(X) ->
			lists:keyfind(X#item.id, 2, New) =/= false
		end,
		Current
	) ++
	lists:filter(
		fun(X) ->
			lists:keyfind(X#item.id, 2, Current) == false
		end,
		New
	).
