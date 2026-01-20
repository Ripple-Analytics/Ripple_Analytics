%%%-------------------------------------------------------------------
%%% @doc Bayesian Analysis Handler
%%% 
%%% Handles Bayesian analysis requests for mental model confidence scoring.
%%% Uses Bayes' theorem to calculate posterior probabilities.
%%% @end
%%%-------------------------------------------------------------------
-module(bayesian_handler).
-behaviour(cowboy_handler).

-export([init/2]).

%%--------------------------------------------------------------------
%% @doc Handle Bayesian analysis request
%% @end
%%--------------------------------------------------------------------
init(Req0, State) ->
    Method = cowboy_req:method(Req0),
    
    case Method of
        <<"POST">> ->
            {ok, Body, Req1} = cowboy_req:read_body(Req0),
            handle_bayesian(Body, Req1, State);
        <<"OPTIONS">> ->
            Req = cowboy_req:reply(200, cors_headers(), <<>>, Req0),
            {ok, Req, State};
        _ ->
            Req = cowboy_req:reply(405, cors_headers(), 
                jsx:encode(#{<<"error">> => <<"Method not allowed">>}), Req0),
            {ok, Req, State}
    end.

handle_bayesian(Body, Req0, State) ->
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
                    %% Get models from registry
                    Models = model_registry:get_all_models(),
                    
                    %% Run Bayesian analysis
                    Result = bayesian_analyzer:analyze(Text, Models),
                    
                    %% Limit to top N results
                    AllModels = maps:get(<<"models">>, Result, []),
                    TopModels = lists:sublist(AllModels, TopN),
                    
                    FinalResult = Result#{<<"models">> => TopModels},
                    
                    Req2 = cowboy_req:reply(200, cors_headers(),
                        jsx:encode(FinalResult), Req0),
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
