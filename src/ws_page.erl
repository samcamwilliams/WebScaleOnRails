-module(ws_page).
-export([main/1]).
-include("ws_recs.hrl").
-include("/usr/local/lib/yaws/include/yaws_api.hrl").

create_pstate(A, InitLog) ->
	IP = kbk_lib:extract_ip(A),
	LogID = kbk_lib:log(wsor, site, IP, [InitLog], A),
	Account = case yaws_api:find_cookie_val("user", (A#arg.headers)#headers.cookie) of
		[] -> undefined;
		Cookie ->
			case kbk_lib:rpc(ds_accounts, login, [Cookie]) of
				{ok, UserID, Token, _} ->
					kbk_lib:add(LogID, {login_from_cookie, UserID}),
					{logged_in, UserID, Token, element(2, kbk_lib:rpc(ds_accounts, get_username, [UserID]))};
				_ -> undefined
			end
	end,
	BaseS = #pstate{ arg = A, entry = InitLog, logid = LogID, ip = IP },
	S = 
		case Account of
			undefined -> BaseS;
			{logged_in, FUserID, FToken, Username} ->
				BaseS#pstate {
					userid = FUserID,
					token = FToken,
					username = Username
				}
		end,
	S.

main(A) ->
	S = create_pstate(A, homepage),
	{html, [],
		[
			{head, [],
				[
					{script, [{type, "text/javascript"}], [ejs:eval(web_ajax:base())]},
					{'link',
						[{rel, "stylesheet"},{type, "text/css"}, {href, "main.css"}], []
					},
					{'link',
						[{rel, "stylesheet"}, {type, "text/css"}, {href, "http://fonts.googleapis.com/css?family=Noto+Sans"}]
					},
					{'link',
						[{rel, "shortcut icon"}, {href, "/imgs/favicon.png"}]
					},
					{title, [], ["Web Scale On Rails"]}
				]
			},
			{body, [{id, "body"}],
				[
					{'div', [{id, "content"}], body(S)}
				]
			}
		]
	}.

body(S) ->
	{'div', [],
		[
			header(S),
			content(S),
			about(S)
		]
	}.

intro(S) ->
	{'div', [],
		case {yaws_api:find_cookie_val("intro", ((S#pstate.arg)#arg.headers)#headers.cookie), S#pstate.nointro} of
			{"seen", _} -> [];
			{[], false} -> [{'div', [], [intro_text(S)]}];
			{_, _} -> []
		end
	}.

intro_text(S) ->
	{'div', [{id, "introcont"}],
		[
			{img,
				[
					{id, "closex"},
					{onclick,
						web_ajax:request(
							fun() ->
								ws:log(S, dismiss_intro),
								[
									{to, "titlecont", header_btn(S#pstate{ nointro = true })},
									{exec, ejst:setDisplayById("introcont", "none")},
									{cookies, [{"intro", "seen"}]}
								]
							end
						)
					},
					{src, "/imgs/cross.png"}
				]
			},
			{'div',
				[{class, "introtext"}], ["Our community is great, but sometimes we're all guilty of pretentious wording or silly blog post titles. Hand out upvotes to links guilty of silly titles to show people the errors of their ways."]
			}
		]
	}.

about(S) ->
	{'div', [{id, "overlay"}, {style, "display: none;"}],
		[
			{'div', [{id, "overlaycenter"}],
				[
					{img,
						[
							{id, "crossimg"},
							{src, "/imgs/cross.png"},
							{onclick,
								web_ajax:request(
									fun() ->
										ws:log(S, {about, hide}),
										{exec, ejst:setDisplayById("overlay", "none")}
									end
								)
							}
						]
					},
					{'div',
						[{id, "overlaytitle"}], ["About"]
					},
					{'div',
						[{id, "overlaytext"}], ["Blah blah blah don't hate us we are prettychillguys blah blahb lasfladjhfd sdghsd;g dfsd]#."]
					}
				]
			}
		]
	}.

header(S) ->
	{'div', [{id, "topbanner"}],
		[
			{'div',
				[{id, "acronymbox"}], ["W"]
			},
			{'div', [{id, "titlecont"}],
				[header_btn(S)]
			},
			{a,
				[
					{id, "about"},
					{href, "javascript:" ++
						web_ajax:request(
							fun() ->
								ws:log(S, {about, show}),
								{exec, ejst:setDisplayById("overlay", "block")}
							end
						)
					}
				],
				["About"]
			}
		]
	}.

header_btn(S) ->
	{a,
		[
			{id, "pagetitle"},
			{href, "javascript:" ++
				web_ajax:request(
					fun() ->
						ws:log(S, homepage),
						{to, "content", [body(S)]}
					end
				)
			}
		],
		[
			"Web Scale on Rails News"
		]
	}.

content(S) ->
	{'div', [{class, "linkscont"}],
		[
			intro(S)
		] ++
		[
			{'div', [{class, "listitem"}],
				[
					{'div',	[{class, "rank"}], [integer_to_list(Num)]}
				] ++
				[
					{img,
						[
							{id, "up_" ++ integer_to_list(X#item.id)},
							{class, "upicon"},
							{src, "/imgs/up.png"},
							{onclick,
								web_ajax:request(
									fun() ->
										case lists:member(S#pstate.ip, X#item.votes) of
											false ->
												ws:log(S, {up, X#item.id}),
												ws_server ! {up, X#item.id, S#pstate.ip },
												{exec, ejst:setVisibilityById("up_" ++ integer_to_list(X#item.id), "hidden")};
											true ->
												ws:log(S, {hack, already_voted})
										end
									end
								)
							}
						] ++
						case lists:member(S#pstate.ip, X#item.votes) of
							false -> [];
							true -> [{style, "visibility: hidden;"}]
						end
					}
				] ++
				[
					{'div',
						[{class, "info"}],
						[		
							{a,
								[{href, X#item.url}, {class, "title"}], [X#item.title]
							},
							{'div',
								[{class, "subtitle"}], ["(" ++ X#item.site ++ ")"]
							},
							{'div',
								[{class, "postdetails"}],
								[
									{'div',
										[{class, "pointscount"}], [X#item.points]
									},
									{'div',
										[{class, "desc"}], ["HN points by"]
									},
									{'div',
										[{class, "authorname"}], [X#item.user]
									},
									{'div',
										[{class, "hourscount"}], [X#item.timeago]
									},
									{'div',
										[{class, "separator"}], [" | "]
									},
									{'div',
										[{class, "commentscount"}], [X#item.comments]
									},
									{'div',
										[{class, "desc"}], ["HN comments"]
									}
								]
							}
						]
					}
				]
			}
		||
			{Num, X} <- number(ws_server:get())
		]
	}.

number(Is) -> number(Is, 1).

number([], _) -> [];
number([H|T], Num) ->
	[{Num, H}] ++ number(T, Num + 1).
