-module(deployment_health_handler).
-behaviour(cowboy_handler).

-export([init/2]).

init(Req0, State) ->
    Service = cowboy_req:binding(service, Req0),
    
    BlueHealth = check_health(Service, <<"blue">>),
    GreenHealth = check_health(Service, <<"green">>),
    ActiveEnv = deployment_state:get_active_env(Service),
    
    Response = #{
        <<"service">> => Service,
        <<"active_env">> => ActiveEnv,
        <<"blue">> => BlueHealth,
        <<"green">> => GreenHealth,
        <<"timestamp">> => iso8601_timestamp()
    },
    
    Req = cowboy_req:reply(200,
        #{<<"content-type">> => <<"application/json">>},
        jsx:encode(Response),
        Req0),
    {ok, Req, State}.

check_health(Service, Env) ->
    Url = build_health_url(Service, Env),
    StartTime = erlang:monotonic_time(millisecond),
    
    Result = case hackney:request(get, Url, [], <<>>, [{timeout, 5000}]) of
        {ok, 200, _, ClientRef} ->
            {ok, Body} = hackney:body(ClientRef),
            EndTime = erlang:monotonic_time(millisecond),
            #{
                <<"healthy">> => true,
                <<"status_code">> => 200,
                <<"response_time_ms">> => EndTime - StartTime,
                <<"response">> => try jsx:decode(Body, [return_maps]) catch _:_ -> Body end
            };
        {ok, Status, _, _} ->
            EndTime = erlang:monotonic_time(millisecond),
            #{
                <<"healthy">> => false,
                <<"status_code">> => Status,
                <<"response_time_ms">> => EndTime - StartTime,
                <<"error">> => <<"Unhealthy status code">>
            };
        {error, Reason} ->
            EndTime = erlang:monotonic_time(millisecond),
            #{
                <<"healthy">> => false,
                <<"status_code">> => null,
                <<"response_time_ms">> => EndTime - StartTime,
                <<"error">> => iolist_to_binary(io_lib:format("~p", [Reason]))
            }
    end,
    
    Result.

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
