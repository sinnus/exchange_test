-module(client_dispatcher).

-include("rfc4627.hrl").
-include("common.hrl").

-export([send_transaction/1,
	 send_request_added/1,
	 send_event_vo/3]).

send_transaction(Transaction) ->
    send_event_vo(Transaction#transaction.buy_user, <<"BuyRequestDone">>, Transaction#transaction.price),
    send_event_vo(Transaction#transaction.sell_user, <<"SellRequestDone">>, Transaction#transaction.price),
    ok.

send_request_added(Principal) ->
    send_event_vo(Principal, <<"RequestAdded">>, none).

send_event_vo(Principal, EventType, JsonData) ->
    EventVO = #event_vo{type = EventType,
			data = JsonData},

    Data = rfc4627:encode(?RFC4627_FROM_RECORD(event_vo, EventVO)),

    error_logger:info_msg("EventVO: ~p~n", [Data]),

    tcp_connection_manager:send_message(Principal, Data).
