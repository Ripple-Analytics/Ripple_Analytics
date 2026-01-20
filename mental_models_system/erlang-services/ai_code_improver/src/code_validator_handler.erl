-module(code_validator_handler).
-behaviour(cowboy_handler).

-export([init/2]).

init(Req0, State) ->
    {ok, Body, Req1} = cowboy_req:read_body(Req0),
    try
        Request = jsx:decode(Body, [return_maps]),
        Code = binary_to_list(maps:get(<<"code">>, Request)),
        
        ValidationPrompt = design_philosophy:get_validation_prompt(Code),
        
        Req2 = case lm_studio_client:generate(ValidationPrompt, "You are a code validator.") of
            {ok, ValidationResponse} ->
                Response = jsx:encode(#{
                    <<"status">> => <<"success">>,
                    <<"validation">> => ValidationResponse
                }),
                cowboy_req:reply(200,
                    #{<<"content-type">> => <<"application/json">>},
                    Response,
                    Req1);
            {error, Reason} ->
                Response = jsx:encode(#{<<"error">> => list_to_binary(io_lib:format("Validation error: ~p", [Reason]))}),
                cowboy_req:reply(500,
                    #{<<"content-type">> => <<"application/json">>},
                    Response,
                    Req1)
        end,
        {ok, Req2, State}
    catch
        _:_ ->
            Req3 = cowboy_req:reply(400,
                #{<<"content-type">> => <<"application/json">>},
                jsx:encode(#{<<"error">> => <<"Invalid JSON">>}),
                Req1),
            {ok, Req3, State}
    end.
