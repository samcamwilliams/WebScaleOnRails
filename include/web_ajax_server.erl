-module(web_ajax_server).
-export([start/0, code_change/0, stop/0, add/1, add/2, add/3, remove/1, clear/0, server/1]).
-export([in_progress_count/0, completed_count/0]).
-define(TIMEOUT_H, 6).

% This really needs upgrading to a record based system.

start() ->
	register(web_ajax_server, spawn(web_ajax_server, server, [{0, 0, dict:new()}])).

stop() ->
	web_ajax_server ! stop.

clear() ->
	web_ajax_server ! clear.

in_progress_count() ->
	web_ajax_server ! {in_progress, self()},
	receive
		{in_progress_count, Count} -> Count
	end.

completed_count() ->
	web_ajax_server ! {completed, self()},
	receive
		{completed_count, Count} -> Count
	end.

add(Fun) ->
	web_ajax_server:add(Fun, infinity).
add(Fun, TTL) ->
	add(Fun, TTL, []).
add(Fun, TTL, Opts) ->
	web_ajax_server ! {add, ID = get_unused(), Fun, TTL, Opts, unix_ts() + (60 *60 * ?TIMEOUT_H)},
	ID.

remove(ID) ->
	web_ajax_server ! {remove, self(), ID},
	receive
		{func, ID, Fun, Opts} -> {Fun, Opts}
	end.

get_unused() ->
	web_ajax_server ! {unused, self(), X = crypto:rand_uniform(1, 10000000000000000)},
	receive
		ok -> X;
		false -> io:format("web_ajax_server: ~w in use!~n", [X]), get_unused()
	end.

unix_ts() ->
	{M, S, _} = now(),
	M*1000000 + S.

code_change() ->
	web_ajax_server ! code_change.

server(State = {Completed, In_progress, FDict}) ->
	FunDict = 
		case (Completed rem 250) of
			0 -> dict:filter(fun(_, {_, _, _, TO}) -> TS = unix_ts(), if TS > TO -> false; true -> true end end, FDict);
			_ -> FDict
		end,
	receive
		{unused, PID, Num} -> PID ! case dict:is_key(Num, FunDict) of true -> false; false -> ok end, server(State);
		code_change -> web_ajax_server:server(State);
		stop -> done;
		clear -> server({Completed, 0, dict:new()});
		{in_progress, PID} -> PID ! {in_progress_count, In_progress}, server(State);
		{completed, PID} -> PID ! {completed_count, Completed}, server(State);
		{add, ID, Fun, TTL, Opts, Timeout} -> server({Completed, In_progress + 1, dict:store(ID, {Fun, TTL, Opts, Timeout}, FunDict)});
		{remove, PID, ID} ->
			case dict:find(ID, FunDict) of
				{ok, {Fun, TTL, Opts, Timeout}} ->
					PID ! {func, ID, Fun, Opts},
					case TTL of
						infinity -> server({Completed+1, In_progress, FunDict});
						1 -> server({Completed+1, In_progress-1, dict:erase(ID, FunDict)});
						Num -> server({Completed+1, In_progress, dict:store(ID, {Fun, Num-1, Opts, Timeout}, FunDict)})
					end;
				error ->
					PID ! {func, ID, fun() -> ns_error:ajax({fun_not_found, ID}) end, []},
					server({Completed+1, In_progress, FunDict})
 			end
	end.
