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

-record(state, {socket, principal, tref, buffer}).
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
				    tref = TRef,
				    buffer = <<>>}}.

wait_for_principal(<<"<policy-file-request/>", 0>>, State) ->
    gen_tcp:send(State#state.socket, <<"<?xml version=\"1.0\" ?>  
   <cross-domain-policy> 
   <site-control permitted-cross-domain-policies=\"all\" secure=\"false\" />  
   <allow-access-from domain=\"*\" to-ports=\"*\" secure=\"false\" />  
   </cross-domain-policy>",0>>),
    {stop, normal, State};

wait_for_principal(Data, State) when is_binary(Data) ->
    Buffer1 = State#state.buffer,
    Buffer2 = <<Buffer1/binary, Data/binary>>,
    {Bin, Rest} = binary_utils:decode_string(Buffer2),
    State1 = State#state{buffer = Rest},
    case Bin of
	<<>> ->
	    {next_state, wait_for_principal, State1};
	_ ->
	    gen_fsm:cancel_timer(State1#state.tref),
	    LoginJsonData =  rfc4627:decode(Bin),
	    case LoginJsonData of
		{ok, JsonData, _R} ->
		    LoginVO = ?RFC4627_TO_RECORD(login_vo, JsonData),
		    Login =  LoginVO#login_vo.login,
		    Password = LoginVO#login_vo.password,
		    case auth_module:check_principal(Login, Password) of
			true ->
			    State2 = State1#state{principal = Login},
			    tcp_connection_manager:add_principal_connection(Login, self()),
			    
			    gen_tcp:send(State1#state.socket, <<"{\"type\": \"Logon\", \"data\": null}",0>>),
			    
			    {next_state, ready, State2};
			false ->
			    error_logger:info_msg("Wrong login:~p~n", [Login]),
			    {stop, normal, State1}
		    end;
		{error, _Reason} ->
		    {stop, normal, State1};
		undefined ->
		    {stop, normal, State1}
	    end
    end;

wait_for_principal(_Data, State) ->
    {stop, normal, State}.

ready(<<"quit\r\n">>, State) ->
    {stop, normal, State};

ready(Data, State) when is_binary(Data) ->
    Buffer1 = State#state.buffer,
    Buffer2 = <<Buffer1/binary, Data/binary>>,
    Rest = process_json_data(Buffer2, State#state.principal),
    State1 = State#state{buffer = Rest},
    {next_state, ready, State1};

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

process_json_data(Buffer, Principal) when is_binary(Buffer) ->
    Rest = binary_utils:process_message(Buffer,
					fun(Message) ->
						do_process_json_data(Message,Principal)
					end),
    Rest.

do_process_json_data(Data, Principal) ->
    JsonData =  rfc4627:decode(Data),
    case JsonData of
	    {ok, Json, _R} ->
		EventVO = ?RFC4627_TO_RECORD(event_vo, Json),
		apply(tcp_connection_callback, handle_request,
		      [Principal,
		       EventVO#event_vo.type,
		       EventVO#event_vo.data]),
		ok;
	    {error, _Reason} ->
		%% TODO: Send error message to client
		ok
    end.
