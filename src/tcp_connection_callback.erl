-module(tcp_connection_callback).

-export([handle_request/3]).

-include("rfc4627.hrl").
-include("common.hrl").

%%----------- RPC Callbacks --------------
handle_request(Principal, <<"RequestBuy">>, Data) ->
    RequestDataVO = ?RFC4627_TO_RECORD(request_data_vo, Data),

    request_server:add_buy_request(request_tool1_server, "user1", 10),

    error_logger:info_msg("RequestBuy:~p~n", [RequestDataVO]),
    ok;

handle_request(_Principal, _Type, _Data) ->
    ok.
