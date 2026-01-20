-module(rollback_handler).
-behaviour(cowboy_handler).

-export([init/2]).

init(Req0, State) ->
    Method = cowboy_req:method(Req0),
    Path = cowboy_req:path(Req0),
    case {Method, Path} of
        {<<"GET">>, <<"/api/rollback/list">>} ->
            handle_list_backups(Req0, State);
        {<<"GET">>, <<"/api/rollback/count">>} ->
            handle_get_count(Req0, State);
        {<<"GET">>, <<"/api/rollback/", BackupId/binary>>} ->
            handle_get_backup(BackupId, Req0, State);
        {<<"POST">>, <<"/api/rollback/create">>} ->
            handle_create_backup(Req0, State);
        {<<"POST">>, <<"/api/rollback/", BackupId/binary, "/restore">>} ->
            handle_restore(BackupId, Req0, State);
        {<<"POST">>, <<"/api/rollback/cleanup">>} ->
            handle_cleanup(Req0, State);
        _ ->
            Req = cowboy_req:reply(404,
                #{<<"content-type">> => <<"application/json">>},
                jsx:encode(#{<<"error">> => <<"Not found">>}),
                Req0),
            {ok, Req, State}
    end.

handle_list_backups(Req0, State) ->
    case rollback_manager:get_backups() of
        {ok, Backups} ->
            Req = cowboy_req:reply(200,
                #{<<"content-type">> => <<"application/json">>},
                jsx:encode(#{<<"backups">> => Backups, <<"count">> => length(Backups)}),
                Req0);
        {error, Reason} ->
            Req = cowboy_req:reply(500,
                #{<<"content-type">> => <<"application/json">>},
                jsx:encode(#{<<"error">> => format_error(Reason)}),
                Req0)
    end,
    {ok, Req, State}.

handle_get_count(Req0, State) ->
    case rollback_manager:get_backup_count() of
        {ok, Count} ->
            Req = cowboy_req:reply(200,
                #{<<"content-type">> => <<"application/json">>},
                jsx:encode(#{<<"count">> => Count}),
                Req0);
        {error, Reason} ->
            Req = cowboy_req:reply(500,
                #{<<"content-type">> => <<"application/json">>},
                jsx:encode(#{<<"error">> => format_error(Reason)}),
                Req0)
    end,
    {ok, Req, State}.

handle_get_backup(BackupId, Req0, State) ->
    case rollback_manager:get_backup(BackupId) of
        {ok, Backup} ->
            Req = cowboy_req:reply(200,
                #{<<"content-type">> => <<"application/json">>},
                jsx:encode(Backup),
                Req0);
        {error, not_found} ->
            Req = cowboy_req:reply(404,
                #{<<"content-type">> => <<"application/json">>},
                jsx:encode(#{<<"error">> => <<"Backup not found">>}),
                Req0);
        {error, Reason} ->
            Req = cowboy_req:reply(500,
                #{<<"content-type">> => <<"application/json">>},
                jsx:encode(#{<<"error">> => format_error(Reason)}),
                Req0)
    end,
    {ok, Req, State}.

handle_create_backup(Req0, State) ->
    {ok, Body, Req1} = cowboy_req:read_body(Req0),
    try
        Request = jsx:decode(Body, [return_maps]),
        FilePath = binary_to_list(maps:get(<<"file">>, Request)),
        
        case rollback_manager:save_backup(FilePath) of
            {ok, BackupInfo} ->
                Req = cowboy_req:reply(200,
                    #{<<"content-type">> => <<"application/json">>},
                    jsx:encode(BackupInfo),
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

handle_restore(BackupId, Req0, State) ->
    case rollback_manager:rollback(BackupId) of
        {ok, Result} ->
            Req = cowboy_req:reply(200,
                #{<<"content-type">> => <<"application/json">>},
                jsx:encode(Result),
                Req0);
        {error, not_found} ->
            Req = cowboy_req:reply(404,
                #{<<"content-type">> => <<"application/json">>},
                jsx:encode(#{<<"error">> => <<"Backup not found">>}),
                Req0);
        {error, Reason} ->
            Req = cowboy_req:reply(500,
                #{<<"content-type">> => <<"application/json">>},
                jsx:encode(#{<<"error">> => format_error(Reason)}),
                Req0)
    end,
    {ok, Req, State}.

handle_cleanup(Req0, State) ->
    case rollback_manager:cleanup_old_backups() of
        {ok, Result} ->
            Req = cowboy_req:reply(200,
                #{<<"content-type">> => <<"application/json">>},
                jsx:encode(Result),
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
