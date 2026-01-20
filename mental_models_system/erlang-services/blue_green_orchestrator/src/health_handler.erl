-module(health_handler).
-behaviour(cowboy_handler).

-export([init/2]).

init(Req0, State) ->
    Response = #{
        <<"status">> => <<"healthy">>,
        <<"service">> => <<"blue-green-orchestrator">>,
        <<"deployment_env">> => list_to_binary(os:getenv("DEPLOYMENT_ENV", "unknown")),
        <<"timestamp">> => iso8601_timestamp()
    },
    
    Req = cowboy_req:reply(200,
        #{<<"content-type">> => <<"application/json">>},
        jsx:encode(Response),
        Req0),
    {ok, Req, State}.

iso8601_timestamp() ->
    {{Y, M, D}, {H, Mi, S}} = calendar:universal_time(),
    iolist_to_binary(io_lib:format("~4..0B-~2..0B-~2..0BT~2..0B:~2..0B:~2..0BZ", [Y, M, D, H, Mi, S])).
