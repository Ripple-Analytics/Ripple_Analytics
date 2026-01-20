%%%-------------------------------------------------------------------
%%% @doc API Proxy Handler - Proxies requests to backend services
%%% 
%%% Routes API requests from the desktop UI to the appropriate backend
%%% microservices. Handles path translation and request forwarding.
%%% 
%%% Path format: /api/{service}/{rest...}
%%% Example: /api/analysis/models -> http://analysis-service:8001/api/analysis/models
%%%-------------------------------------------------------------------
-module(api_proxy_handler).
-behaviour(cowboy_handler).

-export([init/2]).

-define(SERVICES, #{
    <<"gateway">> => "http://api-gateway:8000",
    <<"analysis">> => "http://analysis-service:8001",
    <<"harvester">> => "http://harvester-service:8002",
    <<"storage">> => "http://storage-service:8003",
    <<"chaos">> => "http://chaos-engineering:8005"
}).

init(Req0, State) ->
    Path = cowboy_req:path(Req0),
    Method = cowboy_req:method(Req0),
    
    %% Parse path: /api/{service}/{rest}
    case binary:split(Path, <<"/">>, [global, trim_all]) of
        [<<"api">>, Service | Rest] ->
            case maps:get(Service, ?SERVICES, undefined) of
                undefined ->
                    reply_error(404, <<"Unknown service">>, Req0, State);
                BaseUrl ->
                    %% Reconstruct the full path including /api/{service}/ prefix
                    %% This ensures the backend service receives the expected route
                    TargetPath = case Rest of
                        [] -> "/health";
                        _ -> 
                            %% Keep the full path: /api/{service}/{rest}
                            "/api/" ++ binary_to_list(Service) ++ "/" ++ 
                            binary_to_list(iolist_to_binary(lists:join("/", Rest)))
                    end,
                    proxy_request(Method, BaseUrl ++ TargetPath, Req0, State)
            end;
        _ ->
            reply_error(400, <<"Invalid API path">>, Req0, State)
    end.

proxy_request(Method, Url, Req0, State) ->
    %% Get request body if present
    {ok, Body, Req1} = case Method of
        <<"GET">> -> {ok, <<>>, Req0};
        <<"DELETE">> -> {ok, <<>>, Req0};
        _ -> cowboy_req:read_body(Req0)
    end,
    
    HackneyMethod = case Method of
        <<"GET">> -> get;
        <<"POST">> -> post;
        <<"PUT">> -> put;
        <<"DELETE">> -> delete;
        _ -> get
    end,
    
    Headers = [{<<"content-type">>, <<"application/json">>}],
    Options = [{timeout, 30000}, {recv_timeout, 30000}],
    
    %% Log the proxy request for debugging
    io:format("[PROXY] ~s ~s~n", [Method, Url]),
    
    case hackney:request(HackneyMethod, list_to_binary(Url), Headers, Body, Options) of
        {ok, Status, RespHeaders, ClientRef} ->
            {ok, RespBody} = hackney:body(ClientRef),
            ContentType = proplists:get_value(<<"content-type">>, RespHeaders, <<"application/json">>),
            Req = cowboy_req:reply(Status, #{
                <<"content-type">> => ContentType,
                <<"access-control-allow-origin">> => <<"*">>
            }, RespBody, Req1),
            {ok, Req, State};
        {error, Reason} ->
            io:format("[PROXY ERROR] ~p~n", [Reason]),
            reply_error(502, list_to_binary(io_lib:format("Proxy error: ~p", [Reason])), Req1, State)
    end.

reply_error(Status, Message, Req0, State) ->
    Req = cowboy_req:reply(Status, #{
        <<"content-type">> => <<"application/json">>,
        <<"access-control-allow-origin">> => <<"*">>
    }, jsx:encode(#{<<"error">> => Message}), Req0),
    {ok, Req, State}.
