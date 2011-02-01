%%%-------------------------------------------------------------------
%%% File    : request_server.erl
%%% Author  : Kirill Trofimov <sinnus@gmail.com>
%%% Description : 
%%%
%%% Created :  1 Feb 2011 by Kirill Trofimov <sinnus@gmail.com>
%%%-------------------------------------------------------------------
-module(request_server).

-behaviour(gen_server).
-include("common.hrl").

%% API
-export([start_link/1,
	 stop/1,
	 add_buy_request/3,
	 add_sell_request/3,
	 get_top10_requests/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-record(state, {tool_name,
	        requests_buy,
		requests_sell,
		price_buy2count,
		price_sell2count}).
%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link(ToolName) ->
    gen_server:start_link(?MODULE, [ToolName], []).

stop(Pid) ->
    gen_server:cast(Pid, stop).

%%--------------------------------------------------------------------
%% Description: Add buy request
%%--------------------------------------------------------------------
add_buy_request(Pid, UserName, Price) ->
    gen_server:call(Pid, {add_buy_request, UserName, Price}).

add_sell_request(Pid, UserName, Price) ->
    gen_server:call(Pid, {add_sell_request, UserName, Price}).

get_top10_requests(Pid) ->
    gen_server:call(Pid, {get_top10_requests}).

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
init([ToolName]) ->
    {ok, #state{tool_name = ToolName,
		requests_buy = [],
		requests_sell = [],
		price_buy2count = dict:new(),
		price_sell2count = dict:new()
	       }}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call({add_buy_request, UserName, Price}, _From, State) ->
    Requests_Buy = State#state.requests_buy,
    Request = #request{created_date = erlang:now(),
		       user_name = UserName,
		       type = buy,
		       price = Price},

    case try_make_transaction(Request) of
	true ->
	    {reply, ok, State};
	false ->
	    Dict1 = dict:update_counter(Price, 1, State#state.price_buy2count),
	    State1 = State#state{requests_buy = [Request|Requests_Buy],
				 price_buy2count = Dict1},
	    {reply, ok, State1}
    end;

%% TODO: Remove code duplication
handle_call({add_sell_request, UserName, Price}, _From, State) ->
    Requests_Sell = State#state.requests_sell,
    Request = #request{created_date = erlang:now(),
		       user_name = UserName,
		       type = sell,
		       price = Price},
    
    case try_make_transaction(Request) of
	true ->
	    {reply, ok, State};
	false ->
	    Dict1 = dict:update_counter(Price, 1, State#state.price_sell2count),
	    State1 = State#state{requests_buy = [Request|Requests_Sell],
				 price_sell2count = Dict1},
	    {reply, ok, State1}
    end;

handle_call({get_top10_requests}, _From, State) ->
    Top10BuyList = fetch_top10_requests(State#state.price_buy2count, asc),
    Top10SellList = fetch_top10_requests(State#state.price_sell2count, desc),

    Reply = {Top10BuyList, Top10SellList},
    {reply, Reply, State};

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

try_make_transaction(Request) when Request#request.type == sell->
    false;

try_make_transaction(Request) when Request#request.type == buy->
    false.

fetch_top10_requests(Dict, Order) ->
    PriceCountList = dict:to_list(Dict),
    SortedList = lists:sort(fun({Price1, _}, {Price2, _}) ->
				    case Order of
					asc ->
					    Price1 =< Price2;
					desc ->
					    Price2 =< Price1
				    end
			    end, PriceCountList),
    
    Top10List = lists:sublist(SortedList, 10),
    
    Top10PriceCountList =  lists:map(fun({Price, Count}) ->
					     #request_price_count{price = Price, count = Count}
				     end, Top10List),
    Top10PriceCountList.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
