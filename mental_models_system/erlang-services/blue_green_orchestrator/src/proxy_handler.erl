-module(proxy_handler).
-behaviour(cowboy_handler).

-export([init/2]).

init(Req0, State) ->
    Path = cowboy_req:path(Req0),
    Method = cowboy_req:method(Req0),
    
    Service = extract_service(Path),
    ActiveEnv = deployment_state:get_active_env(Service),
    
    TargetUrl = build_target_url(Service, ActiveEnv, Path),
    
    {ok, Body, Req1} = case Method of
        <<"GET">> -> {ok, <<>>, Req0};
        <<"HEAD">> -> {ok, <<>>, Req0};
        _ -> cowboy_req:read_body(Req0)
    end,
    
    Headers = cowboy_req:headers(Req1),
    HeaderList = maps:to_list(Headers),
    
    case hackney:request(binary_to_atom(Method, utf8), TargetUrl, HeaderList, Body, [{timeout, 30000}]) of
        {ok, Status, RespHeaders, ClientRef} ->
            {ok, RespBody} = hackney:body(ClientRef),
            FilteredHeaders = filter_headers(RespHeaders),
            Req = cowboy_req:reply(Status, maps:from_list(FilteredHeaders), RespBody, Req1),
            {ok, Req, State};
        {error, Reason} ->
            ErrorResponse = #{
                <<"error">> => <<"Proxy error">>,
                <<"reason">> => iolist_to_binary(io_lib:format("~p", [Reason])),
                <<"service">> => Service,
                <<"target_env">> => ActiveEnv
            },
            Req = cowboy_req:reply(502,
                #{<<"content-type">> => <<"application/json">>},
                jsx:encode(ErrorResponse),
                Req1),
            {ok, Req, State}
    end.

extract_service(Path) ->
    case binary:split(Path, <<"/">>, [global, trim_all]) of
        [<<"api">>, Service | _] -> Service;
        [Service | _] -> Service;
        _ -> <<"api-gateway">>
    end.

build_target_url(Service, ActiveEnv, Path) ->
    Port = service_port(Service),
    Host = <<Service/binary, "-", ActiveEnv/binary>>,
    iolist_to_binary([<<"http://">>, Host, <<":">>, integer_to_binary(Port), Path]).

service_port(<<"orchestrator">>) -> 8020;
service_port(<<"api-gateway">>) -> 8000;
service_port(<<"analysis-service">>) -> 8001;
service_port(<<"analysis">>) -> 8001;
service_port(<<"harvester-service">>) -> 8002;
service_port(<<"harvester">>) -> 8002;
service_port(<<"storage-service">>) -> 8003;
service_port(<<"storage">>) -> 8003;
service_port(<<"chaos-engineering">>) -> 8005;
service_port(<<"chaos">>) -> 8005;
service_port(<<"desktop-ui">>) -> 3000;
service_port(<<"ui">>) -> 3000;
service_port(_) -> 8000.

filter_headers(Headers) ->
    SkipHeaders = [<<"transfer-encoding">>, <<"connection">>, <<"keep-alive">>],
    [{K, V} || {K, V} <- Headers, not lists:member(string:lowercase(K), SkipHeaders)].
