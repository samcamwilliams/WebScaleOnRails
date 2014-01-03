-module(ws_scrape).
-export([get_latest/0]).
-include("ws_recs.hrl").
-define(RE, "</td><td class=.title.><a href=.(.+?).>(.+?)<\/a>.+?> \\((.+?)\\).+?>([0-9]+) points.+?href=.user\\?id=(.+?).>.+?</a> (.+?)  \\|.+?.(item\\?id=[0-9]+).>([0-9]+)").

get_latest() ->
	process(extract(download())).

download() -> download(0).

download(3) -> "";
download(X) ->
	case httpc:request("http://news.ycombinator.com/news") of
		{ok, {{_, 200, _}, _, Content}} -> Content;
		_ -> download(X + 1)
	end.

extract(Content) ->
	case re:run(Content, ?RE, [global, {capture, all_but_first, list}]) of
		{match, Matches} -> Matches;
		nomatch -> []
	end.

process(Matches) ->
	lists:map(
		fun([Url, Title, Site, Points, User, TimeAgo, Comurl, Comments]) ->
			#item {
				id = comurl_to_id(Comurl),
				title = Title,
				site = Site,
				url = Url,
				points = Points,
				user = User,
				timeago = TimeAgo,
				comments = Comments
			}
		end,
		Matches
	).

comurl_to_id(Str) ->
	list_to_integer(lists:nth(2, string:tokens(Str, "="))).
