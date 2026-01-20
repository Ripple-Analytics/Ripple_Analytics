-module(exclusion_handler).
-behaviour(cowboy_handler).

-export([init/2]).

init(Req0, State) ->
    Method = cowboy_req:method(Req0),
    case Method of
        <<"GET">> ->
            handle_get_patterns(Req0, State);
        <<"POST">> ->
            handle_update_patterns(Req0, State);
        <<"DELETE">> ->
            handle_reset_patterns(Req0, State);
        _ ->
            Req = cowboy_req:reply(405,
                #{<<"content-type">> => <<"application/json">>},
                jsx:encode(#{<<"error">> => <<"Method not allowed">>}),
                Req0),
            {ok, Req, State}
    end.

handle_get_patterns(Req0, State) ->
    case exclusion_config:get_patterns() of
        {ok, Patterns} ->
            Req = cowboy_req:reply(200,
                #{<<"content-type">> => <<"application/json">>},
                jsx:encode(#{<<"patterns">> => Patterns, <<"count">> => length(Patterns)}),
                Req0);
        {error, Reason} ->
            Req = cowboy_req:reply(500,
                #{<<"content-type">> => <<"application/json">>},
                jsx:encode(#{<<"error">> => format_error(Reason)}),
                Req0)
    end,
    {ok, Req, State}.

handle_update_patterns(Req0, State) ->
    {ok, Body, Req1} = cowboy_req:read_body(Req0),
    try
        Request = jsx:decode(Body, [return_maps]),
        case maps:get(<<"action">>, Request, <<"set">>) of
            <<"add">> ->
                Pattern = maps:get(<<"pattern">>, Request),
                exclusion_config:add_pattern(Pattern),
                Req = cowboy_req:reply(200,
                    #{<<"content-type">> => <<"application/json">>},
                    jsx:encode(#{<<"status">> => <<"added">>, <<"pattern">> => Pattern}),
                    Req1);
            <<"remove">> ->
                Pattern = maps:get(<<"pattern">>, Request),
                exclusion_config:remove_pattern(Pattern),
                Req = cowboy_req:reply(200,
                    #{<<"content-type">> => <<"application/json">>},
                    jsx:encode(#{<<"status">> => <<"removed">>, <<"pattern">> => Pattern}),
                    Req1);
            <<"set">> ->
                Patterns = maps:get(<<"patterns">>, Request),
                exclusion_config:set_patterns(Patterns),
                Req = cowboy_req:reply(200,
                    #{<<"content-type">> => <<"application/json">>},
                    jsx:encode(#{<<"status">> => <<"updated">>, <<"count">> => length(Patterns)}),
                    Req1);
            <<"check">> ->
                FilePath = binary_to_list(maps:get(<<"file">>, Request)),
                IsExcluded = exclusion_config:is_excluded(FilePath),
                Req = cowboy_req:reply(200,
                    #{<<"content-type">> => <<"application/json">>},
                    jsx:encode(#{<<"file">> => list_to_binary(FilePath), <<"excluded">> => IsExcluded}),
                    Req1);
            _ ->
                Req = cowboy_req:reply(400,
                    #{<<"content-type">> => <<"application/json">>},
                    jsx:encode(#{<<"error">> => <<"Invalid action">>}),
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

handle_reset_patterns(Req0, State) ->
    case exclusion_config:reset_to_defaults() of
        ok ->
            Req = cowboy_req:reply(200,
                #{<<"content-type">> => <<"application/json">>},
                jsx:encode(#{<<"status">> => <<"reset_to_defaults">>}),
                Req0);
        {error, Reason} ->
            Req = cowboy_req:reply(500,
                #{<<"content-type">> => <<"application/json">>},
                jsx:encode(#{<<"error">> => format_error(Reason)}),
                Req0)
    end,
    {ok, Req, State}.

format_error(Reason) when is_binary(Reason) -> Reason;
format_error(Reason) when is_atom(Reason) -> atom_to_binary(Reason, utf8);
format_error(Reason) -> list_to_binary(io_lib:format("~p", [Reason])).
