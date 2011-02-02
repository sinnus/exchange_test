-module(transaction_server_tests).

-include_lib("eunit/include/eunit.hrl").
-include("common.hrl").

transaction_create_test() ->
    {ok, _} = transaction_server:start_link(),

    BuyRequest = #request{id = 1,
			  created_date = erlang:now(),
			  user_name = "user1",
			  type = buy,
			  price = 10},

    SellRequest = #request{id = 2,
			   created_date = erlang:now(),
			   user_name = "user2",
			   type = sell,
			   price = 11},

    transaction_server:create_transaction(BuyRequest, SellRequest),

    {ok, UserTrans1} = transaction_server:get_transactions("user1"),
    {ok, UserTrans1} = transaction_server:get_transactions("user2"),

    transaction_server:stop().
