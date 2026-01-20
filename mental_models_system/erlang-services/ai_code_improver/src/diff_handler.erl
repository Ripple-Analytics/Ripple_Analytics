-module(diff_handler).
-behaviour(cowboy_handler).

-export([init/2]).

init(Req0, State) ->
    Method = cowboy_req:method(Req0),
    Path = cowboy_req:path(Req0),
    case {Method, Path} of
        {<<"GET">>, <<"/api/diff/pending">>} ->
            handle_get_pending(Req0, State);
        {<<"GET">>, <<"/api/diff/", DiffId/binary>>} ->
            handle_get_diff(DiffId, Req0, State);
        {<<"POST">>, <<"/api/diff/create">>} ->
            handle_create_diff(Req0, State);
        {<<"POST">>, <<"/api/diff/", DiffId/binary, "/approve">>} ->
            handle_approve(DiffId, Req0, State);
        {<<"POST">>, <<"/api/diff/", DiffId/binary, "/reject">>} ->
            handle_reject(DiffId, Req0, State);
        {<<"DELETE">>, <<"/api/diff/pending">>} ->
            handle_clear_pending(Req0, State);
        _ ->
            Req = cowboy_req:reply(404,
                #{<<"content-type">> => <<"application/json">>},
                jsx:encode(#{<<"error">> => <<"Not found">>}),
                Req0),
            {ok, Req, State}
    end.

handle_get_pending(Req0, State) ->
    case diff_preview:get_pending_diffs() of
        {ok, Diffs} ->
            Req = cowboy_req:reply(200,
                #{<<"content-type">> => <<"application/json">>},
                jsx:encode(#{<<"pending">> => Diffs, <<"count">> => length(Diffs)}),
                Req0);
        {error, Reason} ->
            Req = cowboy_req:reply(500,
                #{<<"content-type">> => <<"application/json">>},
                jsx:encode(#{<<"error">> => format_error(Reason)}),
                Req0)
    end,
    {ok, Req, State}.

handle_get_diff(DiffId, Req0, State) ->
    case diff_preview:get_diff(DiffId) of
        {ok, Diff} ->
            Req = cowboy_req:reply(200,
                #{<<"content-type">> => <<"application/json">>},
                jsx:encode(Diff),
                Req0);
        {error, not_found} ->
            Req = cowboy_req:reply(404,
                #{<<"content-type">> => <<"application/json">>},
                jsx:encode(#{<<"error">> => <<"Diff not found">>}),
                Req0);
        {error, Reason} ->
            Req = cowboy_req:reply(500,
                #{<<"content-type">> => <<"application/json">>},
                jsx:encode(#{<<"error">> => format_error(Reason)}),
                Req0)
    end,
    {ok, Req, State}.

handle_create_diff(Req0, State) ->
    {ok, Body, Req1} = cowboy_req:read_body(Req0),
    try
        Request = jsx:decode(Body, [return_maps]),
        FilePath = binary_to_list(maps:get(<<"file">>, Request)),
        NewContent = maps:get(<<"content">>, Request),
        
        case diff_preview:create_diff(FilePath, NewContent) of
            {ok, DiffInfo} ->
                Req = cowboy_req:reply(200,
                    #{<<"content-type">> => <<"application/json">>},
                    jsx:encode(DiffInfo),
                    Req1);
            {error, Reason} ->
                Req = cowboy_req:reply(500,
                    #{<<"content-type">> => <<"application/json">>},
                    jsx:encode(#{<<"error">> => format_error(Reason)}),
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

handle_approve(DiffId, Req0, State) ->
    case diff_preview:approve_diff(DiffId) of
        {ok, applied} ->
            Req = cowboy_req:reply(200,
                #{<<"content-type">> => <<"application/json">>},
                jsx:encode(#{<<"status">> => <<"applied">>, <<"id">> => DiffId}),
                Req0);
        {error, not_found} ->
            Req = cowboy_req:reply(404,
                #{<<"content-type">> => <<"application/json">>},
                jsx:encode(#{<<"error">> => <<"Diff not found">>}),
                Req0);
        {error, Reason} ->
            Req = cowboy_req:reply(500,
                #{<<"content-type">> => <<"application/json">>},
                jsx:encode(#{<<"error">> => format_error(Reason)}),
                Req0)
    end,
    {ok, Req, State}.

handle_reject(DiffId, Req0, State) ->
    case diff_preview:reject_diff(DiffId) of
        {ok, rejected} ->
            Req = cowboy_req:reply(200,
                #{<<"content-type">> => <<"application/json">>},
                jsx:encode(#{<<"status">> => <<"rejected">>, <<"id">> => DiffId}),
                Req0);
        {error, not_found} ->
            Req = cowboy_req:reply(404,
                #{<<"content-type">> => <<"application/json">>},
                jsx:encode(#{<<"error">> => <<"Diff not found">>}),
                Req0);
        {error, Reason} ->
            Req = cowboy_req:reply(500,
                #{<<"content-type">> => <<"application/json">>},
                jsx:encode(#{<<"error">> => format_error(Reason)}),
                Req0)
    end,
    {ok, Req, State}.

handle_clear_pending(Req0, State) ->
    case diff_preview:clear_pending() of
        ok ->
            Req = cowboy_req:reply(200,
                #{<<"content-type">> => <<"application/json">>},
                jsx:encode(#{<<"status">> => <<"cleared">>}),
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
