-module(request_tests).

-include_lib("eunit/include/eunit.hrl").
-include("common.hrl").

add_get_test() ->
    {ok, Pid} = request_server:start_link("tool1"),

    %% Buy test
    ok = request_server:add_buy_request(Pid, "user1", 10),

    {Top10BuyRequests1, []} = request_server:get_top10_requests(Pid),

    [Request1] = Top10BuyRequests1,

    ?assert(Request1#request_price_count.price =:= 10),
    ?assert(Request1#request_price_count.count =:= 1),

    ok = request_server:add_buy_request(Pid, "user2", 10),
    ok = request_server:add_buy_request(Pid, "user3", 10),
    ok = request_server:add_buy_request(Pid, "user1", 23),

    {Top10BuyRequests2, []} = request_server:get_top10_requests(Pid),

    [Request2, Request3] = Top10BuyRequests2,

    ?assert(Request2#request_price_count.price =:= 10),
    ?assert(Request2#request_price_count.count =:= 3),

    ?assert(Request3#request_price_count.price =:= 23),
    ?assert(Request3#request_price_count.count =:= 1),

    ok = request_server:add_buy_request(Pid, "user1", 1),
    ok = request_server:add_buy_request(Pid, "user1", 2),
    ok = request_server:add_buy_request(Pid, "user1", 3),
    ok = request_server:add_buy_request(Pid, "user1", 4),
    ok = request_server:add_buy_request(Pid, "user1", 5),
    ok = request_server:add_buy_request(Pid, "user1", 6),
    ok = request_server:add_buy_request(Pid, "user1", 7),
    ok = request_server:add_buy_request(Pid, "user1", 8),
    ok = request_server:add_buy_request(Pid, "user1", 9),
    ok = request_server:add_buy_request(Pid, "user1", 10),

    {Top10BuyRequests3, []} = request_server:get_top10_requests(Pid),
    ?assert(10 =:= length(Top10BuyRequests3)),

    Request4 = lists:nth(1, Top10BuyRequests3),
    ?assert(Request4#request_price_count.price =:= 1),
    ?assert(Request4#request_price_count.count =:= 1),

    Request5 = lists:nth(10, Top10BuyRequests3),
    ?assert(Request5#request_price_count.price =:= 10),
    ?assert(Request5#request_price_count.count =:= 4),
    
    %% Sell test
    ok = request_server:add_sell_request(Pid, "user1", 666),
    {_, [Request6]} = request_server:get_top10_requests(Pid),
    ?assert(Request6#request_price_count.price =:= 666),
    ?assert(Request6#request_price_count.count =:= 1),

    ok = request_server:add_sell_request(Pid, "user1", 666),
    ok = request_server:add_sell_request(Pid, "user1", 111),

    {_, [Request7, Request8]} = request_server:get_top10_requests(Pid),
    ?assert(Request7#request_price_count.price =:= 666),
    ?assert(Request7#request_price_count.count =:= 2),
    ?assert(Request8#request_price_count.price =:= 111),
    ?assert(Request8#request_price_count.count =:= 1),

    request_server:stop(Pid).
