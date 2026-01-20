-module(improvement_suggester_handler).
-behaviour(cowboy_handler).

-export([init/2]).

init(Req0, State) ->
    {ok, Body, Req1} = cowboy_req:read_body(Req0),
    try
        Request = jsx:decode(Body, [return_maps]),
        FilePath = binary_to_list(maps:get(<<"file">>, Request)),
        
        case file:read_file(FilePath) of
            {ok, Content} ->
                case code_analyzer:analyze_file(FilePath) of
                    {ok, Analysis} ->
                        CodeContext = io_lib:format("File: ~s~nAnalysis: ~p~nContent:~n~s", 
                                                    [FilePath, Analysis, Content]),
                        Prompt = design_philosophy:get_improvement_prompt(lists:flatten(CodeContext)),
                        SystemPrompt = design_philosophy:get_system_prompt(),
                        
                        case lm_studio_client:generate(Prompt, SystemPrompt) of
                            {ok, Suggestions} ->
                                Response = jsx:encode(#{
                                    <<"status">> => <<"success">>,
                                    <<"file">> => list_to_binary(FilePath),
                                    <<"suggestions">> => Suggestions
                                }),
                                Req = cowboy_req:reply(200,
                                    #{<<"content-type">> => <<"application/json">>},
                                    Response,
                                    Req1);
                            {error, Reason} ->
                                Response = jsx:encode(#{<<"error">> => list_to_binary(io_lib:format("LM Studio error: ~p", [Reason]))}),
                                Req = cowboy_req:reply(500,
                                    #{<<"content-type">> => <<"application/json">>},
                                    Response,
                                    Req1)
                        end;
                    {error, Reason} ->
                        Response = jsx:encode(#{<<"error">> => list_to_binary(io_lib:format("Analysis error: ~p", [Reason]))}),
                        Req = cowboy_req:reply(500,
                            #{<<"content-type">> => <<"application/json">>},
                            Response,
                            Req1)
                end;
            {error, Reason} ->
                Response = jsx:encode(#{<<"error">> => list_to_binary(io_lib:format("File read error: ~p", [Reason]))}),
                Req = cowboy_req:reply(404,
                    #{<<"content-type">> => <<"application/json">>},
                    Response,
                    Req1)
        end,
        {ok, Req, State}
    catch
        _:_ ->
            Req = cowboy_req:reply(400,
                #{<<"content-type">> => <<"application/json">>},
                jsx:encode(#{<<"error">> => <<"Invalid JSON">>}),
                Req1),
            {ok, Req, State}
    end.
