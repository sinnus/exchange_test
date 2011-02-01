-module(request_tests).

-include_lib("eunit/include/eunit.hrl").
-include("common.hrl").

server_test() ->
    {ok, Pid} = request_server:start_link("tool1"),

    ok = request_server:add_buy_request(Pid, "user1", 10),

    {Top10BuyRequests1, []} = request_server:get_top10_requests(Pid),

    [Request1] = Top10BuyRequests1,

    ?assert(Request1#request_price_count.price =:= 10),
    ?assert(Request1#request_price_count.count =:= 1),

    ok = request_server:add_buy_request(Pid, "user2", 10),
    ok = request_server:add_buy_request(Pid, "user1", 23),

    {Top10BuyRequests2, []} = request_server:get_top10_requests(Pid),

    [Request2, Request3] = Top10BuyRequests2,

    ?assert(Request2#request_price_count.price =:= 10),
    ?assert(Request2#request_price_count.count =:= 2),

    ?assert(Request3#request_price_count.price =:= 23),
    ?assert(Request3#request_price_count.count =:= 1),

    request_server:stop(Pid).
