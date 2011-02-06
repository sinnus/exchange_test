%%%-------------------------------------------------------------------
-module(transaction_server).

-behaviour(gen_server).
-include("common.hrl").
%% API
-export([start_link/0,
	 create_transaction/4,
	 get_transactions/1,
	 stop/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-record(state, {transactions}).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() ->
    gen_server:call(?MODULE, stop).

create_transaction(BuyRequest, SellRequest, ToolName, ExistedRequestType) ->
    gen_server:call(?MODULE, {create_transaction, BuyRequest, SellRequest, ToolName, ExistedRequestType}).

get_transactions(User) ->
    gen_server:call(?MODULE, {get_transactions, User}).

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
init([]) ->
    {ok, #state{transactions = []}}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call({create_transaction, BuyRequest, SellRequest, ToolName, ExistedRequestType}, _From, State) ->
    TransactionPrice = case ExistedRequestType of
			   buy ->
			       BuyRequest#request.price;
			   sell ->
			       SellRequest#request.price
		       end,

    Transaction = #transaction{created_date = erlang:now(),
			       tool_name = ToolName,
			       price = TransactionPrice,
			       buy_user = BuyRequest#request.user_name,
			       buy_price = BuyRequest#request.price,
			       sell_user = SellRequest#request.user_name,
			       sell_price = SellRequest#request.price},

    client_dispatcher:send_transaction(Transaction),

    State1 = State#state{transactions = [Transaction|State#state.transactions]},
    {reply, ok, State1};

handle_call({get_transactions, User}, _From, State) ->
    UserTransactions = lists:filter(fun(Elem) ->
					    Elem#transaction.buy_user == User orelse
						Elem#transaction.sell_user == User 
				    end, State#state.transactions),

    Reply = {ok, UserTransactions},
    {reply, Reply, State};

handle_call(stop, _From, State) ->
    {stop, normal, ok, State};

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

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
