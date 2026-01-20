-module(deployment_services_handler).
-behaviour(cowboy_handler).

-export([init/2]).

init(Req0, State) ->
    Services = deployment_state:get_all_services(),
    
    ServiceList = lists:map(fun({Name, Data}) ->
        ActiveEnv = maps:get(<<"active_env">>, Data, <<"blue">>),
        LastSwitch = maps:get(<<"last_switch">>, Data, <<"unknown">>),
        PreviousEnv = maps:get(<<"previous_env">>, Data, null),
        
        #{
            <<"name">> => Name,
            <<"active_env">> => ActiveEnv,
            <<"last_switch">> => LastSwitch,
            <<"previous_env">> => PreviousEnv,
            <<"blue_healthy">> => check_health(Name, <<"blue">>),
            <<"green_healthy">> => check_health(Name, <<"green">>)
        }
    end, maps:to_list(Services)),
    
    Response = #{
        <<"status">> => <<"ok">>,
        <<"services">> => ServiceList,
        <<"total_services">> => length(ServiceList),
        <<"timestamp">> => iso8601_timestamp()
    },
    
    Req = cowboy_req:reply(200,
        #{<<"content-type">> => <<"application/json">>},
        jsx:encode(Response),
        Req0),
    {ok, Req, State}.

check_health(Service, Env) ->
    Url = build_health_url(Service, Env),
    case hackney:request(get, Url, [], <<>>, [{timeout, 2000}]) of
        {ok, 200, _, _} -> true;
        _ -> false
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
