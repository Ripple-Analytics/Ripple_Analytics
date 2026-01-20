%%%-------------------------------------------------------------------
%%% @doc Patterns Handler
%%% 
%%% Handles pattern extraction requests including:
%%% - Recurring patterns in text
%%% - Key insights extraction
%%% - Inverted perspective generation (Inversion mental model)
%%% @end
%%%-------------------------------------------------------------------
-module(patterns_handler).
-behaviour(cowboy_handler).

-export([init/2]).

%%--------------------------------------------------------------------
%% @doc Handle pattern extraction request
%% @end
%%--------------------------------------------------------------------
init(Req0, State) ->
    Method = cowboy_req:method(Req0),
    
    case Method of
        <<"POST">> ->
            {ok, Body, Req1} = cowboy_req:read_body(Req0),
            handle_patterns(Body, Req1, State);
        <<"OPTIONS">> ->
            Req = cowboy_req:reply(200, cors_headers(), <<>>, Req0),
            {ok, Req, State};
        _ ->
            Req = cowboy_req:reply(405, cors_headers(), 
                jsx:encode(#{<<"error">> => <<"Method not allowed">>}), Req0),
            {ok, Req, State}
    end.

handle_patterns(Body, Req0, State) ->
    try jsx:decode(Body, [return_maps]) of
        Params ->
            Text = maps:get(<<"text">>, Params, <<>>),
            Action = maps:get(<<"action">>, Params, <<"extract">>),
            case Text of
                <<>> ->
                    Req1 = cowboy_req:reply(400, cors_headers(),
                        jsx:encode(#{<<"error">> => <<"No text provided">>}), Req0),
                    {ok, Req1, State};
                _ ->
                    Result = case Action of
                        <<"extract">> ->
                            pattern_extractor:extract_patterns(Text);
                        <<"insights">> ->
                            pattern_extractor:extract_key_insights(Text);
                        <<"invert">> ->
                            pattern_extractor:generate_inverted_perspective(Text);
                        <<"all">> ->
                            Patterns = pattern_extractor:extract_patterns(Text),
                            Insights = pattern_extractor:extract_key_insights(Text),
                            Inversions = pattern_extractor:generate_inverted_perspective(Text),
                            #{
                                <<"success">> => true,
                                <<"patterns">> => maps:get(<<"patterns">>, Patterns, []),
                                <<"insights">> => maps:get(<<"insights">>, Insights, []),
                                <<"inversions">> => maps:get(<<"inversions">>, Inversions, []),
                                <<"munger_quote">> => maps:get(<<"munger_quote">>, Inversions, <<>>)
                            };
                        _ ->
                            #{<<"error">> => <<"Unknown action">>}
                    end,
                    
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
