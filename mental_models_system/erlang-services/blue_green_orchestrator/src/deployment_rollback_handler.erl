-module(deployment_rollback_handler).
-behaviour(cowboy_handler).

-export([init/2]).

init(Req0, State) ->
    Method = cowboy_req:method(Req0),
    Service = cowboy_req:binding(service, Req0),
    handle_request(Method, Service, Req0, State).

handle_request(<<"POST">>, Service, Req0, State) ->
    Result = rollback_service(Service),
    
    Response = case Result of
        {ok, PreviousEnv} ->
            #{
                <<"status">> => <<"success">>,
                <<"message">> => <<"Rollback completed successfully">>,
                <<"service">> => Service,
                <<"rolled_back_to">> => PreviousEnv,
                <<"timestamp">> => iso8601_timestamp()
            };
        {error, no_previous_env} ->
            #{
                <<"status">> => <<"error">>,
                <<"message">> => <<"No previous environment to rollback to">>,
                <<"service">> => Service,
                <<"timestamp">> => iso8601_timestamp()
            };
        {error, Reason} ->
            #{
                <<"status">> => <<"error">>,
                <<"message">> => iolist_to_binary(io_lib:format("~p", [Reason])),
                <<"service">> => Service,
                <<"timestamp">> => iso8601_timestamp()
            }
    end,
    
    StatusCode = case Result of
        {ok, _} -> 200;
        {error, _} -> 500
    end,
    
    Req = cowboy_req:reply(StatusCode,
        #{<<"content-type">> => <<"application/json">>},
        jsx:encode(Response),
        Req0),
    {ok, Req, State};

handle_request(_, _, Req0, State) ->
    Req = cowboy_req:reply(405,
        #{<<"content-type">> => <<"application/json">>},
        jsx:encode(#{<<"error">> => <<"Method not allowed">>}),
        Req0),
    {ok, Req, State}.

rollback_service(Service) ->
    Status = deployment_state:get_service_status(Service),
    case maps:get(<<"previous_env">>, Status, undefined) of
        undefined -> {error, no_previous_env};
        PreviousEnv ->
            deployment_state:set_active_env(Service, PreviousEnv),
            io:format("[ROLLBACK] Rolled back ~s to ~s~n", [Service, PreviousEnv]),
            {ok, PreviousEnv}
    end.

iso8601_timestamp() ->
    {{Y, M, D}, {H, Mi, S}} = calendar:universal_time(),
    iolist_to_binary(io_lib:format("~4..0B-~2..0B-~2..0BT~2..0B:~2..0B:~2..0BZ", [Y, M, D, H, Mi, S])).
