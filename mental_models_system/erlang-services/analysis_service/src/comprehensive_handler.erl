%%%-------------------------------------------------------------------
%%% @doc Comprehensive Analysis Handler
%%% 
%%% Handles comprehensive analysis requests including:
%%% - Mental model detection
%%% - Lollapalooza effect detection
%%% - Failure mode analysis
%%% - Actionable recommendations
%%% @end
%%%-------------------------------------------------------------------
-module(comprehensive_handler).
-behaviour(cowboy_handler).

-export([init/2]).

%%--------------------------------------------------------------------
%% @doc Handle comprehensive analysis request
%% @end
%%--------------------------------------------------------------------
init(Req0, State) ->
    Method = cowboy_req:method(Req0),
    
    case Method of
        <<"POST">> ->
            {ok, Body, Req1} = cowboy_req:read_body(Req0),
            handle_comprehensive(Body, Req1, State);
        <<"OPTIONS">> ->
            Req = cowboy_req:reply(200, cors_headers(), <<>>, Req0),
            {ok, Req, State};
        _ ->
            Req = cowboy_req:reply(405, cors_headers(), 
                jsx:encode(#{<<"error">> => <<"Method not allowed">>}), Req0),
            {ok, Req, State}
    end.

handle_comprehensive(Body, Req0, State) ->
    try jsx:decode(Body, [return_maps]) of
        Params ->
            Text = maps:get(<<"text">>, Params, <<>>),
            TopN = maps:get(<<"top_n">>, Params, 10),
            case Text of
                <<>> ->
                    Req1 = cowboy_req:reply(400, cors_headers(),
                        jsx:encode(#{<<"error">> => <<"No text provided">>}), Req0),
                    {ok, Req1, State};
                _ ->
                    Result = lollapalooza_detector:analyze_comprehensive(Text, TopN),
                    Req2 = cowboy_req:reply(200, cors_headers(),
                        jsx:encode(Result), Req0),
                    {ok, Req2, State}
            end
    catch
        _:_ ->
            Req3 = cowboy_req:reply(400, cors_headers(),
                jsx:encode(#{<<"error">> => <<"Invalid JSON">>}), Req0),
            {ok, Req3, State}
    end.

cors_headers() ->
    #{<<"content-type">> => <<"application/json">>,
      <<"access-control-allow-origin">> => <<"*">>,
      <<"access-control-allow-methods">> => <<"GET, POST, OPTIONS">>,
      <<"access-control-allow-headers">> => <<"Content-Type">>}.
