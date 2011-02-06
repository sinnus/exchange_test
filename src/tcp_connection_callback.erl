-module(tcp_connection_callback).

-export([handle_request/3]).

-include("rfc4627.hrl").
-include("common.hrl").

%%----------- RPC Callbacks --------------
handle_request(Principal, <<"RequestBuy">>, Data) ->
    RequestDataVO = ?RFC4627_TO_RECORD(request_data_vo, Data),

    request_server:add_buy_request(request_tool1_server, Principal, RequestDataVO#request_data_vo.price),

    error_logger:info_msg("RequestBuy:~p~n", [RequestDataVO]),
    ok;

handle_request(Principal, <<"RequestSell">>, Data) ->
    RequestDataVO = ?RFC4627_TO_RECORD(request_data_vo, Data),

    request_server:add_sell_request(request_tool1_server, Principal, RequestDataVO#request_data_vo.price),

    error_logger:info_msg("RequestSell:~p~n", [RequestDataVO]),
    ok;

handle_request(Principal, <<"GetRequests">>, _) ->
    {Top10Buy, Top10Sell} = request_server:get_top10_requests(request_tool1_server),
    Top10BuyVOs = convert_request_to_vos(Top10Buy),
    Top10SellVOs = convert_request_to_vos(Top10Sell),

    Top10RequestsVO = #top_10_requests_vo{buy_requests = Top10BuyVOs,
					  sell_requests = Top10SellVOs},

    Top10RequestsJson = ?RFC4627_FROM_RECORD(top_10_requests_vo, Top10RequestsVO),
    
    send_event_vo(Principal, <<"Top10BuySellRequests">>, Top10RequestsJson),
    ok;

handle_request(_Principal, _Type, _Data) ->
    ok.

send_event_vo(Principal, EventType, JsonData) ->
    EventVO = #event_vo{type = EventType,
			data = JsonData},

    Data = rfc4627:encode(?RFC4627_FROM_RECORD(event_vo, EventVO)),

    error_logger:info_msg("EventVO: ~p~n", [Data]),

    tcp_connection_manager:send_message(Principal, Data).

%% Internal funcs
convert_request_to_vos(Top10Requests) ->
    lists:map(fun(Elem) ->
		     ElemVO = #request_price_count_vo{
		       price = Elem#request_price_count.price,
		       count = Elem#request_price_count.count},
		      ?RFC4627_FROM_RECORD(request_price_count_vo, ElemVO)
	     end, Top10Requests).
