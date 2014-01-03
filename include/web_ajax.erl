-module(web_ajax).
-export([request/1, request/2, base/0]).
-define(default_url, "/aj_srv.yaws")
-define(funID_arg_name, "fun").

%% ACCEPTABLE OPTIONS
%%    ttl = Time To Live
%%    ready = Execute this JS after the result is recieved, response body avialable in 'response'
%%    result_to = Send the whole response to this ID
%%    post_call = Executed after the call is made
%%    send_js = JS variables to send, sent as first arguments
%%    send = Input form fields to send, sent after js

base() ->
	{function, ajax_call, [post, exec], 
		lists:flatten([
			{var, xhr},
			{'if', {'==', {["navigator", "appName"]}, {quotes, "Microsoft.XMLHTTP"}},
				[
					{'=', xhr, {new, "ActiveXObject", [{quotes, "Microsoft.XMLHTTP"}]}}
				],
				[
					{'=', xhr, {new, "XMLHttpRequest"}}
				]
			},
			{[xhr, "open"], 
				[
					{quotes, "POST"},
					{quotes, "/aj_srv.yaws"},
					"true"
				]
			},
			{[xhr, "setRequestHeader"], [{quotes, "Content-type"}, {quotes, "application/x-www-form-urlencoded"}]},
			{'=', {[xhr, "onreadystatechange"]},
				{function, [],
					[
						{'if', {'==', {[xhr, "readyState"]}, 4},
							[
								{[eval], [{[xhr, "responseText"]}]}
							]
						}
					]
				}
			},
			{[xhr, "send"], [post]},
			{'if', {'!=', exec, 0},
				[
					{[exec], []}
				]
			}
		])
	}.

request(Fun) -> request(Fun, []).
request(Fun, Opts) ->
	FunID = web_ajax_server:add(Fun, 
		case lists:keyfind(ttl, 1, Opts) of
			{ttl, Val} -> Val;
			false -> infinity
		end, Opts),
	ejs:eval(
		{[ajax_call],
			[
				post_from_opts(FunID, Opts),
				case lists:keyfind(post_call, 1, Opts) of
					{post_call, Statements} ->
						{function, [],
							[
								{statements, Statements}
							]
						};
					false -> 0
				end
			]
		}
	).

post_from_opts(FunID, Opts) ->
	{quotes, lists:flatten([
		"fun=" ++ integer_to_list(FunID) ++
		case lists:keyfind(send_js, 1, Opts) of
			{send_js, Vars_to_send} ->  
				[ "&" ++ Name ++ "='+" ++ Name ++ "+'" || Name <- Vars_to_send ];
			false -> ""
		end,
		case lists:keyfind(send, 1, Opts) of
			{send, Vars_to_send} ->  
				[ "&" ++ Name ++ "='+encodeURIComponent(document.getElementById('" ++ Name ++ "').value)+'" || Name <- Vars_to_send ];
			false -> ""
		end
	])}.
