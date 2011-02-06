%%%-------------------------------------------------------------------
%%% File    : tcp_connection.erl
%%% Author  : sinnus <sinnus@linux>
%%% Description : 
%%%
%%% Created : 11 Nov 2010 by sinnus <sinnus@linux>
%%%-------------------------------------------------------------------
-module(tcp_connection).

-behaviour(gen_fsm).
-include("rfc4627.hrl").
-include("common.hrl").

%% API
-export([start_link/1, start/1,
	 send_message/2]).

%% gen_fsm callbacks
-export([init/1, handle_info/3,
	 handle_event/3, handle_sync_event/4,
	 terminate/3, code_change/4]).

%% gen_fsm states
-export([wait_for_principal/2,
	 ready/2]).

-define(HANDSHAKE_TIMEOUT, 30000).

-record(state, {socket, principal, tref}).
%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link(Socket) ->
    gen_fsm:start_link(?MODULE, [Socket], []).

start(Socket) ->
    gen_fsm:start(?MODULE, [Socket], []).

send_message(Pid, Message) ->
    gen_fsm:send_event(Pid, {send_message, Message}).

%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------
init([Socket]) ->
    activate(Socket),
    TRef = gen_fsm:send_event_after(?HANDSHAKE_TIMEOUT, stop),
    {ok, wait_for_principal, #state{socket = Socket,
				    principal = none,
				    tref = TRef}}.

wait_for_principal(Data, State) when is_binary(Data) ->
    gen_fsm:cancel_timer(State#state.tref),
    LoginJsonData =  try
			 rfc4627:decode(Data)
		     catch
			 error:Reason ->
			     error_logger:info_msg("Couldn't parse json data. Reason: ~p~n", [Reason]),
			     undefined
		     end,
    case LoginJsonData of
	{ok, JsonData, _R} ->
	    LoginVO = ?RFC4627_TO_RECORD(login_vo, JsonData),
	    Login =  LoginVO#login_vo.login,
	    Password = LoginVO#login_vo.password,
	    case auth_module:check_principal(Login, Password) of
		true ->
		    NewState = State#state{principal = Login},
		    tcp_connection_manager:add_principal_connection(Login, self()),

		    tcp_connection_manager:send_message(Login, <<"{\"type\": \"Logon\", \"data\": null}">>),

		    {next_state, ready, NewState};
		false ->
		    error_logger:info_msg("Wrong login:~p~n", [Login]),
		    {stop, normal, State}
	    end;
	{error, _Reason} ->
	    {stop, normal, State};
	undefined ->
	    {stop, normal, State}
    end;

wait_for_principal(_Data, State) ->
    {stop, normal, State}.

ready(<<"quit\r\n">>, State) ->
    {stop, normal, State};

ready(Data, State) when is_binary(Data) ->
    JsonData =  rfc4627:decode(Data),

    R = case JsonData of
	    {ok, Json, _R} ->
		EventVO = ?RFC4627_TO_RECORD(event_vo, Json),
		apply(tcp_connection_callback, handle_request,
		      [State#state.principal,
		       EventVO#event_vo.type,
		       EventVO#event_vo.data]),
		ok;
	    {error, Reason} ->
		%% TODO: Send error message to client
		ok
	end,
    {next_state, ready, State};

ready({send_message, Message}, State) when is_list(Message) ->
    MessageBin = list_to_binary(Message),
    gen_tcp:send(State#state.socket, <<MessageBin/binary, 0>>),
    {next_state, ready, State};

ready({send_message, Message}, State) when is_binary(Message) ->
    gen_tcp:send(State#state.socket, <<Message/binary, 0>>),
    {next_state, ready, State};

ready(Data, State) ->
    error_logger:info_msg("Data:~p~n", [Data]),
    {next_state, ready, State}.

%%--------------------------------------------------------------------
handle_event(_Event, StateName, State) ->
    {next_state, StateName, State}.

handle_sync_event(_Event, _From, StateName, State) ->
    {reply, {error, badarg}, StateName, State}.

%%--------------------------------------------------------------------
handle_info({tcp, Socket, Data}, StateName, #state{socket = Socket} = State) ->
    activate(Socket),
    gen_fsm:send_event(self(), Data),
    {next_state, StateName, State};

handle_info({tcp_closed, _Socket}, _StateName, #state{socket = _Socket} = State) ->
    error_logger:info_msg("connection reset by peer~n", []),
    {stop, normal, State};

handle_info({tcp_error, Reason, Socket}, _StateName, #state{socket = Socket} = State) ->
    error_logger:info_msg("connection error: ~s~n", [inet:format_error(Reason)]),
    {stop, normal, State};

%%handle_info({'DOWN', _Ref, process, Owner, _}, _StateName, State) ->
%%    error_logger:info_msg("DOWN~n", []),
 %%   {stop, normal, State};

handle_info(Info, StateName, State) ->
    error_logger:info_msg("unexpected info in ~p:~n\t~p", [StateName, Info]),
    {next_state, StateName, State}.

%%--------------------------------------------------------------------
terminate(Reason, _StateName, State) ->
    _Socket = State#state.socket,
    Principal = State#state.principal,
    tcp_connection_manager:remove_principal_connection(Principal, self()),
    error_logger:info_msg("terminating, pid=~w, reason=~w", [self(), Reason]),
    ok.

%%--------------------------------------------------------------------
code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
activate(Socket) ->
    inet:setopts(Socket, [{active, once}]).
