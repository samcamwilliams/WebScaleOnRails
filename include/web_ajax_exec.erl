-module(web_ajax_exec).
-export([process_fun/2]).
-define(SUPPORTED_OPTS, [to, exec, ehtml, cookies, status, url, append_to]).
-define(SAVABLE, "pbody").

process_fun(FunID, Args) ->
	try
		begin
			{Fun, Opts} = web_ajax_server:remove(list_to_integer(FunID)),
			Execd = 
				case erlang:fun_info(Fun, arity) of
					{arity, 0} -> Fun();
					{arity, 1} -> Fun([ X || {_, X} <- tl(yaws_api:parse_post(Args))])
				end,
			Ready = lists:flatten(normalize_opts(lists:flatten([Execd]), Opts)),
			process_headers(Ready) ++
			[
				{html, ejs:eval({statements, process_body(Ready)})}
			]
		end of
		X -> X
	catch
		Type:Details ->
			bf:d({Type, Details, erlang:get_stacktrace()}),
			{html, ejs:eval({statements, process_body(op_error:ajax({Type, Details}))})}
	end.

normalize_opts([], Opts) ->
	case lists:keyfind(ready, 1, Opts) of
		{ready, JS} -> [{exec, JS}];
		false -> []
	end;
normalize_opts([{ehtml, Ehtml}|Rest], Opts) ->
	case lists:keyfind(result_to, 1, Opts) of
		{result_to, ID} -> [{to, ID, Ehtml}] ++ normalize_opts(Rest, Opts);
		false -> normalize_opts(Rest, Opts)
	end;
normalize_opts([{eweb, List}|Rest], Opts) ->
	normalize_opts(lists:flatten(List) ++ Rest, Opts);
normalize_opts(String = [First|_], Opts) when is_integer(First) -> % Could be a nice lists:all (or something)
	case lists:keyfind(result_to, 1, Opts) of
		{result_to, ID} -> [{to, ID, String}] ++ normalize_opts([], Opts);
		false -> []
	end;
normalize_opts(All = [Other|Rest], Opts) ->
	case lists:member(element(1, Other), ?SUPPORTED_OPTS) of
		true -> [Other] ++ normalize_opts(Rest, Opts);
		false -> 
			{Usual, Unusual} = get_others(All),
			normalize_opts([{ehtml, Unusual}] ++ Usual, Opts)
	end.

get_others(List) ->
	lists:partition(
		fun(X) -> lists:member(element(1, X), ?SUPPORTED_OPTS) end,
		List).

process_headers([]) -> [];
process_headers([{cookies, Cookies}|Rest]) ->
	[
		{header, {set_cookie, io_lib:format("~s=~s;", [Name, Value])}}
	||
		{Name, Value} <- Cookies
	] ++ process_headers(Rest);
process_headers([{status, Number}|Rest]) ->
	[{status, Number}] ++ process_headers(Rest);
process_headers([Tuple|Rest]) ->
	case lists:member(element(1, Tuple), ?SUPPORTED_OPTS) of
		true -> [];
		false -> [Tuple]
	end ++ process_headers(Rest).

process_body([]) -> [];
process_body([{url, New, Title, Data}]) ->
	[
		{[window, history, pushState], [Data, {quotes, escape(Title)}, {quotes, New}]}
	];
process_body([{url, New, Title}]) ->
	[
		{[window, history, pushState], [ejst:getHTMLById({quotes, ?SAVABLE}), {quotes, escape(Title)}, {quotes, New}]}
	];
process_body([{url, New}]) ->
	[
		{[window, history, pushState], [ejst:getHTMLById({quotes, ?SAVABLE}), {quotes, ""}, {quotes, New}]}
	];
process_body([{to, ID, HTML}|Rest]) ->
	[set_id(ID, HTML)] ++ process_body(Rest);
process_body([{append_to, ID, HTML}|Rest]) ->
	[append_id(ID, HTML)] ++ process_body(Rest);
process_body([{exec, JS}|Rest]) ->
	[JS] ++ process_body(Rest);
process_body([Current|Rest]) when is_tuple(Current) ->
	if element(1, Current) == url -> process_body(Rest ++ [Current]);
		true -> process_body(Rest)
	end;
process_body([_|Rest]) ->
	process_body(Rest).

set_id(ID, Content) ->
	{'=', {[{["document", "getElementById"], [{quotes, ID}]}, "innerHTML"]},
		{["decodeURIComponent"], [{quotes, escape(yaws_api:ehtml_expand(lists:flatten([Content])))}]}
	}.

append_id(ID, Content) ->
	{'+=', {[{["document", "getElementById"], [{quotes, ID}]}, "innerHTML"]},
		{["decodeURIComponent"], [{quotes, escape(yaws_api:ehtml_expand(lists:flatten([Content])))}]}
	}.
escape(String) ->
	yaws_api:url_encode(lists:flatten(String)).
