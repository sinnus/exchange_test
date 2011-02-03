-module(test_suite).

-export([test/0]).

test() ->
    eunit:test(transaction_server_tests),
    eunit:test(request_tests).
