%%%----------------------------------------------------------------------
%%% File    : eventlog.erl
%%% Author  : Ery Lee <ery.lee@gmail.com>
%%% Purpose : evnet logger
%%% Created : 07 Jun 2012
%%% License : http://www.opengoss.com/
%%%
%%% Copyright (C) 2012, www.opengoss.com 
%%%----------------------------------------------------------------------
-module(eventlog).

-author('ery.lee@gmail.com').

-import(erlang, [send_after/3]).

-include("event.hrl").

-include_lib("elog/include/elog.hrl").

-export([start_link/0, stats/0]).

-behaviour(gen_server).

-export([init/1, 
        handle_call/3, 
		prioritise_call/3,
        handle_cast/2, 
        handle_info/2, 
        prioritise_info/2,
        terminate/2, 
        code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {channel}).

start_link() ->
    gen_server2:start_link({local, ?SERVER}, ?MODULE, [], []).

stats() ->
	gen_server2:call(?MODULE, stats).

init([]) ->
	put(received, 0),
	{ok, Conn} = amqp:connect(),
    Channel = open(Conn),
    ?INFO("eventlog is starting...[ok]", []),
    {ok, #state{channel = Channel}}.

open(Conn) ->
	{ok, Channel} = amqp:open_channel(Conn),
	{ok, Q} = amqp:queue(Channel, node()),
	%declare trapd topic
	amqp:topic(Channel, <<"oss.event">>),
	%declare topic and queue
	amqp:bind(Channel, <<"oss.event">>, Q, <<"event.#">>),
	amqp:consume(Channel, Q),
	Channel.

handle_call(stats, _From, State) ->
	Rep = [{received, get(received)}],
	{reply, Rep, State};

handle_call(Req, _From, State) ->
    {stop, {error, {badreq, Req}}, State}.

prioritise_call(stats, _From, _State) ->
	10;
prioritise_call(_, _From, _State) ->
	0.

handle_cast(Msg, State) ->
    {stop, {error, {badmsg, Msg}}, State}.

handle_info({deliver, <<"event.", _Name/binary>>, _Props, Payload}, State) ->
	put(received, get(received) + 1),
	logevent(binary_to_term(Payload)),
    {noreply, State};

handle_info({amqp, disconnected}, State) ->
	{noreply, State#state{channel = undefined}};

handle_info({amqp, reconnected, Conn}, State) ->
	{noreply, State#state{channel = open(Conn)}};

handle_info(Info, State) ->
    {stop, {error, {badinfo, Info}}, State}.

prioritise_info({amqp, disconnected}, _State) ->
    10;
prioritise_info({amqp, reconnected, _}, _State) ->
    10;
prioritise_info(_, _State) ->
    0.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

logevent(#event{name = Name,
				sender = Sender,
				source = Source,
				evtkey = EvtKey,
				severity = Severity,
				summary = Summary,
				timestamp = Time,
				manager = Manager,
				from = From,
				trapoid = TrapOid,
				vars = Vars}) ->
	?INFO("event ~s from ~p", [Name, Sender]),	
	?INFO("source: ~p", [Source]),
	?INFO("evtkey: ~s", [EvtKey]),
	?INFO("severity: ~p", [Severity]),
	?INFO("summary: ~s", [Summary]),
	?INFO("timestamp: ~p", [Time]),
	?INFO("manager: ~p", [Manager]),
	?INFO("from: ~p", [From]),
	?INFO("trapoid: ~s", [TrapOid]),
	[?INFO("var:~s: ~p", [Var, Val]) || {Var, Val} <- Vars],
	?INFO_MSG("end.~n").

