%%%-------------------------------------------------------------------
%%% @doc Proxy Handler
%%% 
%%% Proxies requests to backend microservices with circuit breaker
%%% and retry logic for fault tolerance.
%%% @end
%%%-------------------------------------------------------------------
-module(proxy_handler).
-behaviour(cowboy_handler).

-export([init/2]).

%%--------------------------------------------------------------------
%% @doc Handle proxy request
%% @end
%%--------------------------------------------------------------------
init(Req0, [ServiceType]) ->
    Method = cowboy_req:method(Req0),
    Path = cowboy_req:path(Req0),
    
    %% Get service URL based on type
    ServiceUrl = get_service_url(ServiceType),
    
    %% Build target URL
    TargetPath = strip_prefix(Path, ServiceType),
    TargetUrl = ServiceUrl ++ binary_to_list(TargetPath),
    
    %% Get request body if present
    {ok, Body, Req1} = case Method of
        <<"GET">> -> {ok, <<>>, Req0};
        <<"DELETE">> -> {ok, <<>>, Req0};
        _ -> cowboy_req:read_body(Req0)
    end,
    
    %% Proxy the request with circuit breaker
    case circuit_breaker:call(ServiceType, fun() ->
        proxy_request(Method, TargetUrl, Body)
    end) of
        {ok, {Status, RespHeaders, RespBody}} ->
            Req = cowboy_req:reply(Status,
                maps:merge(RespHeaders, #{
                    <<"access-control-allow-origin">> => <<"*">>
                }),
                RespBody,
                Req1),
            {ok, Req, []};
        {error, circuit_open} ->
            Req = cowboy_req:reply(503,
                #{<<"content-type">> => <<"application/json">>,
                  <<"access-control-allow-origin">> => <<"*">>},
                jsx:encode(#{<<"error">> => <<"Service temporarily unavailable">>}),
                Req1),
            {ok, Req, []};
        {error, Reason} ->
            Req = cowboy_req:reply(502,
                #{<<"content-type">> => <<"application/json">>,
                  <<"access-control-allow-origin">> => <<"*">>},
                jsx:encode(#{<<"error">> => list_to_binary(io_lib:format("~p", [Reason]))}),
                Req1),
            {ok, Req, []}
    end.

%%--------------------------------------------------------------------
%% @doc Get service URL from configuration
%% @end
%%--------------------------------------------------------------------
get_service_url(analysis) ->
    application:get_env(api_gateway, analysis_service_url, "http://analysis-service:8001");
get_service_url(harvester) ->
    application:get_env(api_gateway, harvester_service_url, "http://harvester-service:8002");
get_service_url(storage) ->
    application:get_env(api_gateway, storage_service_url, "http://storage-service:8003").

%%--------------------------------------------------------------------
%% @doc Strip service prefix from path
%% @end
%%--------------------------------------------------------------------
strip_prefix(Path, analysis) ->
    binary:replace(Path, <<"/api/analysis">>, <<"">>);
strip_prefix(Path, harvester) ->
    binary:replace(Path, <<"/api/harvester">>, <<"">>);
strip_prefix(Path, storage) ->
    binary:replace(Path, <<"/api/storage">>, <<"">>).

%%--------------------------------------------------------------------
%% @doc Proxy HTTP request using hackney
%% @end
%%--------------------------------------------------------------------
proxy_request(Method, Url, Body) ->
    Headers = [{<<"content-type">>, <<"application/json">>}],
    Options = [{timeout, 30000}, {recv_timeout, 30000}],
    
    HackneyMethod = case Method of
        <<"GET">> -> get;
        <<"POST">> -> post;
        <<"PUT">> -> put;
        <<"DELETE">> -> delete;
        _ -> get
    end,
    
    case hackney:request(HackneyMethod, list_to_binary(Url), Headers, Body, Options) of
        {ok, Status, RespHeaders, ClientRef} ->
            {ok, RespBody} = hackney:body(ClientRef),
            RespHeadersMap = maps:from_list([{K, V} || {K, V} <- RespHeaders]),
            {ok, {Status, RespHeadersMap, RespBody}};
        {error, Reason} ->
            {error, Reason}
    end.
