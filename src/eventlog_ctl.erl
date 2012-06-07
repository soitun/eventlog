%%%----------------------------------------------------------------------
%%% File    : eventlog_ctl.erl
%%% Author  : Ery Lee <ery.lee@gmail.com>
%%% Purpose : eventlog control
%%% Created : 12 Nov. 2010
%%% License : http://www.opengoss.com
%%%
%%% Copyright (C) 2012, www.opengoss.com
%%%----------------------------------------------------------------------
-module(eventlog_ctl).

-author('ery.lee@gmail.com').

-include_lib("elog/include/elog.hrl").

-compile(export_all).

status() ->
    {InternalStatus, _ProvidedStatus} = init:get_status(),
    ?PRINT("node ~p is ~p.~n", [node(), InternalStatus]),
    case lists:keysearch(eventlog, 1, application:which_applications()) of
    false ->
        ?PRINT("eventlog is not running~n", []);
    {value,_Version} ->
        ?PRINT("eventlog is running~n", [])
    end.

info() ->
	Stats = [Mod:stats() || Mod <- [eventlog]],
	?PRINT("~p~n", [lists:flatten(Stats)]).

