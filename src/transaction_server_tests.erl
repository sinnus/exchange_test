-module(transaction_server_tests).

-include_lib("eunit/include/eunit.hrl").
-include("common.hrl").

transaction_create_test() ->
    {ok, _} = transaction_server:start_link(),
    transaction_server:stop().
