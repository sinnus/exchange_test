-module(tcp_connection_callback).

-export([handle_request/3]).

-include("rfc4627.hrl").
-include("common.hrl").

%%----------- RPC Callbacks --------------
handle_request(Principal, <<"RequestBuy">>, Data) ->
    RequestDataVO = ?RFC4627_TO_RECORD(request_data_vo, Data),

    request_server:add_buy_request(request_tool1_server, Principal, RequestDataVO#request_data_vo.price),
    client_dispatcher:send_request_added(Principal),

    error_logger:info_msg("RequestBuy:~p~n", [RequestDataVO]),
    ok;

handle_request(Principal, <<"RequestSell">>, Data) ->
    RequestDataVO = ?RFC4627_TO_RECORD(request_data_vo, Data),

    request_server:add_sell_request(request_tool1_server, Principal, RequestDataVO#request_data_vo.price),
    client_dispatcher:send_request_added(Principal),

    error_logger:info_msg("RequestSell:~p~n", [RequestDataVO]),
    ok;

handle_request(Principal, <<"GetRequests">>, _) ->
    {Top10Buy, Top10Sell} = request_server:get_top10_requests(request_tool1_server),
    Top10BuyVOs = convert_request_to_vos(Top10Buy),
    Top10SellVOs = convert_request_to_vos(Top10Sell),

    Top10RequestsVO = #top_10_requests_vo{buy_requests = Top10BuyVOs,
					  sell_requests = Top10SellVOs},

    Top10RequestsJson = ?RFC4627_FROM_RECORD(top_10_requests_vo, Top10RequestsVO),
    
    client_dispatcher:send_event_vo(Principal, <<"Top10BuySellRequests">>, Top10RequestsJson),
    ok;

handle_request(Principal, <<"GetTransactions">>, _) ->
    {ok, Transactions} = transaction_server:get_transactions(Principal),
    TransactionVOs = convert_transactions_to_vos(Transactions),
    client_dispatcher:send_event_vo(Principal, <<"Transactions">>, TransactionVOs),
    ok;

handle_request(_Principal, _Type, _Data) ->
    ok.

%% Internal funcs
convert_request_to_vos(Top10Requests) ->
    lists:map(fun(Elem) ->
		     ElemVO = #request_price_count_vo{
		       price = Elem#request_price_count.price,
		       count = Elem#request_price_count.count},
		      ?RFC4627_FROM_RECORD(request_price_count_vo, ElemVO)
	     end, Top10Requests).

convert_transactions_to_vos(Transactions) ->
    lists:map(fun(Elem) ->
		      ElemVO = #transaction_vo{
		       tool_name = list_to_binary(Elem#transaction.tool_name),
		       price = Elem#transaction.price},
		      ?RFC4627_FROM_RECORD(transaction_vo, ElemVO)
	      end, Transactions).
