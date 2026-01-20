-module(deployment_status_handler).
-behaviour(cowboy_handler).

-export([init/2]).

init(Req0, State) ->
    Services = deployment_state:get_all_services(),
    
    Response = #{
        <<"status">> => <<"ok">>,
        <<"orchestrator_env">> => list_to_binary(os:getenv("DEPLOYMENT_ENV", "unknown")),
        <<"services">> => Services,
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
