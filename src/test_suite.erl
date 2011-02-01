-module(test_suite).

-export([test/0]).

test() ->
    eunit:test(request_tests).
