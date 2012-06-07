-module(eventlog_app).

-export([start/0, stop/0]).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

-define(APPS, [sasl,
			   crypto,
			   elog,
			   amqp_client,
			   eventlog
]).

start() ->
	[application:start(App) || App <- ?APPS].

stop() ->
	[application:stop(App) || App <- lists:reverse(?APPS)].
	
%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    eventlog_sup:start_link().

stop(_State) ->
    ok.
