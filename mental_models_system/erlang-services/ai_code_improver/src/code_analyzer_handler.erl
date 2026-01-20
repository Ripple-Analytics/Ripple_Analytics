-module(code_analyzer_handler).
-behaviour(cowboy_handler).

-export([init/2]).

init(Req0, State) ->
    Method = cowboy_req:method(Req0),
    case Method of
        <<"GET">> ->
            handle_get_summary(Req0, State);
        <<"POST">> ->
            handle_analyze(Req0, State);
        _ ->
            Req = cowboy_req:reply(405,
                #{<<"content-type">> => <<"application/json">>},
                jsx:encode(#{<<"error">> => <<"Method not allowed">>}),
                Req0),
            {ok, Req, State}
    end.

handle_get_summary(Req0, State) ->
    case code_analyzer:get_codebase_summary() of
        {ok, Summary} ->
            Response = jsx:encode(Summary),
            Req = cowboy_req:reply(200,
                #{<<"content-type">> => <<"application/json">>},
                Response,
                Req0);
        {error, Reason} ->
            Response = jsx:encode(#{<<"error">> => list_to_binary(io_lib:format("~p", [Reason]))}),
            Req = cowboy_req:reply(500,
                #{<<"content-type">> => <<"application/json">>},
                Response,
                Req0)
    end,
    {ok, Req, State}.

handle_analyze(Req0, State) ->
    {ok, Body, Req1} = cowboy_req:read_body(Req0),
    try
        Request = jsx:decode(Body, [return_maps]),
        Path = binary_to_list(maps:get(<<"path">>, Request, <<"/app/services">>)),
        
        case filelib:is_dir(Path) of
            true ->
                case code_analyzer:analyze_directory(Path) of
                    {ok, Results} ->
                        Response = jsx:encode(#{
                            <<"status">> => <<"success">>,
                            <<"files_analyzed">> => length(Results),
                            <<"results">> => [R || {_, R} <- Results]
                        }),
                        Req = cowboy_req:reply(200,
                            #{<<"content-type">> => <<"application/json">>},
                            Response,
                            Req1);
                    {error, Reason} ->
                        Response = jsx:encode(#{<<"error">> => list_to_binary(io_lib:format("~p", [Reason]))}),
                        Req = cowboy_req:reply(500,
                            #{<<"content-type">> => <<"application/json">>},
                            Response,
                            Req1)
                end;
            false ->
                case code_analyzer:analyze_file(Path) of
                    {ok, Analysis} ->
                        Response = jsx:encode(#{
                            <<"status">> => <<"success">>,
                            <<"analysis">> => Analysis
                        }),
                        Req = cowboy_req:reply(200,
                            #{<<"content-type">> => <<"application/json">>},
                            Response,
                            Req1);
                    {error, Reason} ->
                        Response = jsx:encode(#{<<"error">> => list_to_binary(io_lib:format("~p", [Reason]))}),
                        Req = cowboy_req:reply(500,
                            #{<<"content-type">> => <<"application/json">>},
                            Response,
                            Req1)
                end
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
