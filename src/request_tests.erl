-module(request_tests).

-include_lib("eunit/include/eunit.hrl").
-include("common.hrl").

server_test() ->
    {ok, Pid} = request_server:start_link("tool1"),

    ok = request_server:add_buy_request(Pid, "user1", 10),

    request_server:stop(Pid).
