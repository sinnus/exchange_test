%%%-------------------------------------------------------------------
%%% File    : exchange_application.erl
%%% Author  :  sinnus
%%% Description : 
%%%
%%% Created : 01 Feb 2011 by  sinnus
%%%-------------------------------------------------------------------
-module(exchange_application).

-behaviour(application).

%% Application callbacks
-export([start/0, start/2, stop/1, stop/0]).

start() ->
    ok = application:start(exchange_application).

stop() ->
    application:stop(exchange_application).

%%====================================================================
%% Application callbacks
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start(Type, StartArgs) -> {ok, Pid} |
%%                                     {ok, Pid, State} |
%%                                     {error, Reason}
%% Description: This function is called whenever an application 
%% is started using application:start/1,2, and should start the processes
%% of the application. If the application is structured according to the
%% OTP design principles as a supervision tree, this means starting the
%% top supervisor of the tree.
%%--------------------------------------------------------------------
start(_Type, _StartArgs) ->
    case exchange_sup:start_link() of
	{ok, Pid} -> 
	    {ok, Pid};
	Error ->
	    Error
    end.

%%--------------------------------------------------------------------
%% Function: stop(State) -> void()
%% Description: This function is called whenever an application
%% has stopped. It is intended to be the opposite of Module:start/2 and
%% should do any necessary cleaning up. The return value is ignored. 
%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================
