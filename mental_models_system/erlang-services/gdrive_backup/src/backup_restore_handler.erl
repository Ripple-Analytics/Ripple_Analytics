%%%-------------------------------------------------------------------
%%% @doc Backup Restore Handler
%%% 
%%% Handles restore operations from Google Drive or local backups.
%%% @end
%%%-------------------------------------------------------------------
-module(backup_restore_handler).
-behaviour(cowboy_handler).

-export([init/2]).

init(Req0, State) ->
    Method = cowboy_req:method(Req0),
    
    case Method of
        <<"POST">> ->
            {ok, Body, Req1} = cowboy_req:read_body(Req0),
            handle_restore(Body, Req1, State);
        <<"OPTIONS">> ->
            Req = cowboy_req:reply(200, cors_headers(), <<>>, Req0),
            {ok, Req, State};
        _ ->
            Req = cowboy_req:reply(405, 
                #{<<"content-type">> => <<"application/json">>},
                jsx:encode(#{<<"error">> => <<"Method not allowed">>}), Req0),
            {ok, Req, State}
    end.

handle_restore(Body, Req0, State) ->
    try jsx:decode(Body, [return_maps]) of
        Params ->
            Source = maps:get(<<"source">>, Params, <<>>),
            
            Result = case Source of
                <<>> ->
                    {error, <<"No source specified">>};
                <<"gdrive">> ->
                    Url = maps:get(<<"url">>, Params, <<>>),
                    case Url of
                        <<>> -> {error, <<"No Google Drive URL specified">>};
                        _ -> gdrive_client:download(Url)
                    end;
                <<"local">> ->
                    Path = maps:get(<<"path">>, Params, <<>>),
                    restore_from_local(Path);
                _ ->
                    {error, <<"Unknown source">>}
            end,
            
            Response = case Result of
                {ok, Data} ->
                    #{<<"success">> => true, <<"data">> => Data};
                {error, Reason} ->
                    #{<<"success">> => false, <<"error">> => Reason}
            end,
            
            Req = cowboy_req:reply(200, cors_headers(), jsx:encode(Response), Req0),
            {ok, Req, State}
    catch
        _:_ ->
            Req1 = cowboy_req:reply(400, cors_headers(),
                jsx:encode(#{<<"error">> => <<"Invalid JSON">>}), Req0),
            {ok, Req1, State}
    end.

restore_from_local(Path) when byte_size(Path) > 0 ->
    FullPath = case binary:match(Path, <<"/">>) of
        {0, _} -> binary_to_list(Path);
        _ -> "/data/backups/" ++ binary_to_list(Path)
    end,
    
    case filelib:is_regular(FullPath) of
        true ->
            %% Extract backup
            RestoreDir = "/data/restore_" ++ integer_to_list(erlang:system_time(second)),
            filelib:ensure_dir(RestoreDir ++ "/"),
            
            Cmd = io_lib:format("tar -xzf ~s -C ~s 2>&1", [FullPath, RestoreDir]),
            Output = os:cmd(Cmd),
            
            {ok, #{
                <<"restored_to">> => list_to_binary(RestoreDir),
                <<"source">> => Path,
                <<"output">> => list_to_binary(Output)
            }};
        false ->
            {error, <<"Backup file not found">>}
    end;
restore_from_local(_) ->
    {error, <<"No path specified">>}.

cors_headers() ->
    #{<<"content-type">> => <<"application/json">>,
      <<"access-control-allow-origin">> => <<"*">>,
      <<"access-control-allow-methods">> => <<"POST, OPTIONS">>,
      <<"access-control-allow-headers">> => <<"Content-Type">>}.
