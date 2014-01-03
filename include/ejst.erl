-module(ejst).
-export([getById/1, getById/2, getValueById/1, getValueById/2]).
-export([setHTMLById/2, setHTMLById/3]).
-export([getHTMLById/1, getHTMLById/2]).
-export([getDisplayById/1, setDisplayById/2]).
-export([getVisibilityById/1, setVisibilityById/2]).

getValueById(Name) ->
	getValueById(Name, "document").
getValueById(Name, Domain) ->
	{[getById(Name, Domain), "value"]}.

getById(Name) ->
	getById(Name, "document").
getById(Name, Domain) ->
	{[Domain, "getElementById"], [Name]}.

getHTMLById(Name) ->
	getHTMLById(Name, "document").

getHTMLById(Name, Domain) ->
	{[{[Domain, "getElementById"], [Name]}, "innerHTML"]}.

setHTMLById(Name, Value) ->
	setHTMLById(Name, "document", Value).

setHTMLById(Name, Domain, Value) ->
	{'=', {[{[Domain, "getElementById"], [Name]}, "innerHTML"]}, Value}.

getDisplayById(ID) ->
	{[ejst:getById({quotes, ID}), style, display]}.

setDisplayById(ID, Style) ->
	{'=', {[ejst:getById({quotes, ID}), style, display]}, {quotes, Style}}.

getVisibilityById(ID) ->
	{[ejst:getById({quotes, ID}), style, visibility]}.

setVisibilityById(ID, Style) ->
	{'=', {[ejst:getById({quotes, ID}), style, visibility]}, {quotes, Style}}.
