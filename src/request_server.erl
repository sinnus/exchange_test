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
		price_sell2count,
		seq_value}).
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
		requests_buy = ordsets:new(),
		requests_sell = ordsets:new(),
		price_buy2count = dict:new(),
		price_sell2count = dict:new(),
		seq_value = 1
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
    Request = #request{id = State#state.seq_value,
		       created_date = erlang:now(),
		       user_name = UserName,
		       type = buy,
		       price = Price},

    case try_make_buy_transaction(Request, State) of
	{true, State1} ->
	    {reply, ok, State1};
	{false, State1} ->
	    Dict1 = dict:update_counter(Price, 1, State1#state.price_buy2count),
	    State2 = State1#state{requests_buy = ordsets:add_element(Request, State1#state.requests_buy),
				  price_buy2count = Dict1,
				  seq_value = State1#state.seq_value + 1},
	    {reply, ok, State2}
    end;

%% TODO: Remove code duplication
handle_call({add_sell_request, UserName, Price}, _From, State) ->
    Request = #request{id = State#state.seq_value,
		       created_date = erlang:now(),
		       user_name = UserName,
		       type = sell,
		       price = Price},
    
    case try_make_sell_transaction(Request, State) of
	{true, State1} ->
	    {reply, ok, State1};
	{false, State1} ->
	    Dict1 = dict:update_counter(Price, 1, State1#state.price_sell2count),
	    State2 = State1#state{requests_sell = ordsets:add_element(Request, State1#state.requests_sell),
				  price_sell2count = Dict1,
				  seq_value = State1#state.seq_value + 1},
	    {reply, ok, State2}
    end;

handle_call({get_top10_requests}, _From, State) ->
    Top10BuyList = fetch_top10_requests(State#state.price_buy2count, asc),
    Top10SellList = fetch_top10_requests(State#state.price_sell2count, desc),

    Reply = {Top10BuyList, Top10SellList},
    {reply, Reply, State};

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%% ------------------------
%% Internal funcs
%% ------------------------
try_make_sell_transaction(Request, State) ->
    Sorted = lists:sort(fun(Elem1, Elem2) ->
				{Elem1#request.price, Elem1#request.created_date} =<
				    {Elem2#request.price, Elem2#request.created_date}
			end, ordsets:to_list(State#state.requests_buy)),

    case find_high_price(Request#request.price, Sorted) of
	none ->
	    {false, State};
	BuyRequest ->
	    Dict1 = dict:update_counter(Request#request.price, -1, State#state.price_buy2count),
	    Dict2 = case dict:fetch(Request#request.price, Dict1) of
			0 ->
			    dict:erase(Request#request.price, Dict1);
			_ ->
			    Dict1
		    end,
	    
	    State1 = State#state{requests_buy = ordsets:del_element(BuyRequest,  State#state.requests_buy),
				 price_buy2count = Dict2},
	    {true, State1}
    end.

try_make_buy_transaction(Request, State) ->
    {false, State}.

find_low_price(_Price, []) ->
    none;

find_low_price(Price, [Element|SortedRequests]) ->
    case Element#request.price  =< Price of
	true ->
	    Element;
	false ->
	    find_low_price(Price, SortedRequests)
    end.

find_high_price(_Price, []) ->
    none;

find_high_price(Price, [Element|SortedRequests]) ->
    case Element#request.price  >= Price of
	true ->
	    Element;
	false ->
	    find_high_price(Price, SortedRequests)
    end.

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
