-module(deployment_switch_handler).
-behaviour(cowboy_handler).

-export([init/2]).

init(Req0, State) ->
    Method = cowboy_req:method(Req0),
    Service = cowboy_req:binding(service, Req0, <<"all">>),
    handle_request(Method, Service, Req0, State).

handle_request(<<"POST">>, Service, Req0, State) ->
    {ok, Body, Req1} = cowboy_req:read_body(Req0),
    
    Params = case Body of
        <<>> -> #{};
        _ -> 
            try jsx:decode(Body, [return_maps]) of
                Map -> Map
            catch
                _:_ -> #{}
            end
    end,
    
    TargetEnv = maps:get(<<"target_env">>, Params, <<"toggle">>),
    
    Result = case Service of
        <<"all">> -> switch_all_services(TargetEnv);
        _ -> switch_service(Service, TargetEnv)
    end,
    
    Response = case Result of
        ok ->
            #{
                <<"status">> => <<"success">>,
                <<"message">> => <<"Deployment switched successfully">>,
                <<"service">> => Service,
                <<"target_env">> => TargetEnv,
                <<"timestamp">> => iso8601_timestamp()
            };
        {ok, Details} ->
            #{
                <<"status">> => <<"success">>,
                <<"message">> => <<"Deployment switched successfully">>,
                <<"service">> => Service,
                <<"details">> => Details,
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
        ok -> 200;
        {ok, _} -> 200;
        {error, _} -> 500
    end,
    
    Req = cowboy_req:reply(StatusCode,
        #{<<"content-type">> => <<"application/json">>},
        jsx:encode(Response),
        Req1),
    {ok, Req, State};

handle_request(<<"GET">>, Service, Req0, State) ->
    Status = deployment_state:get_service_status(Service),
    
    Response = #{
        <<"service">> => Service,
        <<"status">> => Status,
        <<"timestamp">> => iso8601_timestamp()
    },
    
    Req = cowboy_req:reply(200,
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

switch_service(Service, <<"toggle">>) ->
    CurrentEnv = deployment_state:get_active_env(Service),
    NewEnv = case CurrentEnv of
        <<"blue">> -> <<"green">>;
        <<"green">> -> <<"blue">>;
        _ -> <<"blue">>
    end,
    switch_service(Service, NewEnv);

switch_service(Service, TargetEnv) ->
    case check_health(Service, TargetEnv) of
        ok ->
            deployment_state:set_active_env(Service, TargetEnv),
            io:format("[SWITCH] Switched ~s to ~s~n", [Service, TargetEnv]),
            ok;
        {error, Reason} ->
            io:format("[SWITCH] Health check failed for ~s (~s): ~p~n", [Service, TargetEnv, Reason]),
            {error, {health_check_failed, Reason}}
    end.

switch_all_services(TargetEnv) ->
    Services = [
        <<"api-gateway">>,
        <<"analysis-service">>,
        <<"harvester-service">>,
        <<"storage-service">>,
        <<"chaos-engineering">>,
        <<"desktop-ui">>,
        <<"orchestrator">>
    ],
    
    Results = lists:map(fun(Service) ->
        Result = switch_service(Service, TargetEnv),
        {Service, Result}
    end, Services),
    
    Failures = [R || R = {_, {error, _}} <- Results],
    case Failures of
        [] -> {ok, Results};
        _ -> {error, {partial_failure, Failures}}
    end.

check_health(Service, Env) ->
    Url = build_health_url(Service, Env),
    case hackney:request(get, Url, [], <<>>, [{timeout, 5000}]) of
        {ok, 200, _, _} -> ok;
        {ok, Status, _, _} -> {error, {unhealthy, Status}};
        {error, Reason} -> {error, Reason}
    end.

build_health_url(Service, Env) ->
    Port = service_port(Service),
    Host = <<Service/binary, "-", Env/binary>>,
    iolist_to_binary([<<"http://">>, Host, <<":">>, integer_to_binary(Port), <<"/health">>]).

service_port(<<"orchestrator">>) -> 8020;
service_port(<<"api-gateway">>) -> 8000;
service_port(<<"analysis-service">>) -> 8001;
service_port(<<"harvester-service">>) -> 8002;
service_port(<<"storage-service">>) -> 8003;
service_port(<<"chaos-engineering">>) -> 8005;
service_port(<<"desktop-ui">>) -> 3000;
service_port(_) -> 8000.

iso8601_timestamp() ->
    {{Y, M, D}, {H, Mi, S}} = calendar:universal_time(),
    iolist_to_binary(io_lib:format("~4..0B-~2..0B-~2..0BT~2..0B:~2..0B:~2..0BZ", [Y, M, D, H, Mi, S])).
