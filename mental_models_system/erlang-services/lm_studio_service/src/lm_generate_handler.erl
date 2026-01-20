-module(lm_generate_handler).
-behaviour(cowboy_handler).
-export([init/2]).

init(Req0, State) ->
    case cowboy_req:method(Req0) of
        <<"POST">> ->
            {ok, Body, Req1} = cowboy_req:read_body(Req0),
            handle_generate(Body, Req1, State);
        <<"OPTIONS">> ->
            Req = cowboy_req:reply(200, cors_headers(), <<>>, Req0),
            {ok, Req, State};
        _ ->
            Req = cowboy_req:reply(405, cors_headers(), 
                jsx:encode(#{<<"error">> => <<"Method not allowed">>}), Req0),
            {ok, Req, State}
    end.

handle_generate(Body, Req0, State) ->
    try jsx:decode(Body, [return_maps]) of
        Params ->
            Prompt = maps:get(<<"prompt">>, Params, <<>>),
            SystemPrompt = maps:get(<<"system_prompt">>, Params, 
                <<"You are a helpful assistant.">>),
            Options = maps:get(<<"options">>, Params, #{}),
            
            case lm_client:generate(Prompt, SystemPrompt, Options) of
                {ok, Response} ->
                    Result = jsx:encode(#{
                        <<"success">> => true,
                        <<"response">> => Response
                    }),
                    Req = cowboy_req:reply(200, cors_headers(), Result, Req0),
                    {ok, Req, State};
                {error, Reason} ->
                    Result = jsx:encode(#{
                        <<"success">> => false,
                        <<"error">> => list_to_binary(io_lib:format("~p", [Reason]))
                    }),
                    Req = cowboy_req:reply(500, cors_headers(), Result, Req0),
                    {ok, Req, State}
            end
    catch
        _:_ ->
            Req = cowboy_req:reply(400, cors_headers(),
                jsx:encode(#{<<"error">> => <<"Invalid JSON">>}), Req0),
            {ok, Req, State}
    end.

cors_headers() ->
    #{<<"content-type">> => <<"application/json">>,
      <<"access-control-allow-origin">> => <<"*">>,
      <<"access-control-allow-methods">> => <<"GET, POST, OPTIONS">>,
      <<"access-control-allow-headers">> => <<"Content-Type">>}.
