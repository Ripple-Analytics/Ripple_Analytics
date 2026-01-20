%%%-------------------------------------------------------------------
%%% @doc gdrive_client Helper Module - Part 2
%%% @end
%%%-------------------------------------------------------------------
-module(gdrive_client_part2).

-export([do_download/2, extract_download_url/1, do_upload/3, do_list_backups/1, load_config/0, save_config/1, save_backup_metadata/1, write_status/1, format_datetime/1, format_datetime/6]).

do_download(FileIdOrUrl, _State) when is_binary(FileIdOrUrl) ->
    %% Determine if it's a URL or file ID
    DownloadUrl = case binary:match(FileIdOrUrl, <<"http">>) of
        {0, _} -> 
            %% It's a URL, extract file ID or use directly
            extract_download_url(FileIdOrUrl);
        _ -> 
            %% It's a file ID
            <<"https://drive.google.com/uc?export=download&id=", FileIdOrUrl/binary>>
    end,
    
    BackupPath = "/data/backups",
    filelib:ensure_dir(BackupPath ++ "/"),
    OutputFile = BackupPath ++ "/backup_" ++ integer_to_list(erlang:system_time(second)) ++ ".zip",
    
    case hackney:request(get, DownloadUrl, [], <<>>, [{timeout, ?TIMEOUT}, {follow_redirect, true}]) of
        {ok, 200, _Headers, ClientRef} ->
            {ok, Body} = hackney:body(ClientRef),
            case file:write_file(OutputFile, Body) of
                ok -> 
                    {ok, #{<<"file">> => list_to_binary(OutputFile), <<"size">> => byte_size(Body)}};
                {error, Reason} -> 
                    {error, {write_failed, Reason}}
            end;
        {ok, Status, _, _} ->
            {error, {http_error, Status}};
        {error, Reason} ->
            {error, Reason}
    end.

extract_download_url(Url) ->
    %% Extract file ID from various Google Drive URL formats
    case re:run(Url, <<"(?:id=|/d/)([a-zA-Z0-9_-]+)">>, [{capture, [1], binary}]) of
        {match, [FileId]} ->
            <<"https://drive.google.com/uc?export=download&id=", FileId/binary>>;
        nomatch ->
            %% Use URL as-is
            Url
    end.

do_upload(FilePath, FileName, State) ->
    case State#state.folder_id of
        <<>> ->
            {error, no_folder_configured};
        FolderId ->
            %% For now, we'll create a simple upload notification
            %% Full Google Drive API upload requires OAuth2
            case file:read_file(FilePath) of
                {ok, Content} ->
                    %% Store locally and record metadata
                    BackupPath = "/data/backups/" ++ binary_to_list(FileName),
                    file:write_file(BackupPath, Content),
                    
                    %% Record backup metadata
                    Metadata = #{
                        <<"name">> => FileName,
                        <<"size">> => byte_size(Content),
                        <<"created">> => format_datetime(calendar:local_time()),
                        <<"folder_id">> => FolderId,
                        <<"local_path">> => list_to_binary(BackupPath)
                    },
                    save_backup_metadata(Metadata),
                    
                    {ok, Metadata};
                {error, Reason} ->
                    {error, {read_failed, Reason}}
            end
    end.

do_list_backups(_State) ->
    BackupPath = "/data/backups",
    case file:list_dir(BackupPath) of
        {ok, Files} ->
            Backups = lists:filtermap(fun(File) ->
                FullPath = BackupPath ++ "/" ++ File,
                case filelib:is_regular(FullPath) of
                    true ->
                        {ok, Info} = file:read_file_info(FullPath),
                        {true, #{
                            <<"name">> => list_to_binary(File),
                            <<"size">> => element(2, Info),
                            <<"modified">> => format_datetime(element(6, Info)),
                            <<"path">> => list_to_binary(FullPath)
                        }};
                    false ->
                        false
                end
            end, Files),
            {ok, Backups};
        {error, enoent} ->
            {ok, []};
        {error, Reason} ->
            {error, Reason}
    end.

load_config() ->
    case file:read_file(?CONFIG_FILE) of
        {ok, Content} ->
            try jsx:decode(Content, [return_maps]) catch _:_ -> #{} end;
        {error, _} ->
            #{}
    end.

save_config(Config) ->
    filelib:ensure_dir(?CONFIG_FILE),
    file:write_file(?CONFIG_FILE, jsx:encode(Config)).

save_backup_metadata(Metadata) ->
    MetadataFile = "/data/backup_metadata.json",
    Existing = case file:read_file(MetadataFile) of
        {ok, Content} ->
            try jsx:decode(Content, [return_maps]) catch _:_ -> #{<<"backups">> => []} end;
        {error, _} ->
            #{<<"backups">> => []}
    end,
    Backups = maps:get(<<"backups">>, Existing, []),
    Updated = Existing#{<<"backups">> => [Metadata | Backups]},
    file:write_file(MetadataFile, jsx:encode(Updated)).

write_status(State) ->
    Status = #{
        <<"status">> => atom_to_binary(State#state.status),
        <<"gdrive_configured">> => byte_size(State#state.gdrive_url) > 0 orelse byte_size(State#state.folder_id) > 0,
        <<"last_sync">> => format_datetime(State#state.last_sync),
        <<"error_message">> => State#state.error_message,
        <<"updated_at">> => format_datetime(calendar:local_time())
    },
    filelib:ensure_dir(?STATUS_FILE),
    file:write_file(?STATUS_FILE, jsx:encode(Status)).

format_datetime(undefined) -> <<"never">>;
format_datetime({{Y, M, D}, {H, Mi, S}}) ->
    list_to_binary(io_lib:format("~4..0B-~2..0B-~2..0BT~2..0B:~2..0B:~2..0B", [Y, M, D, H, Mi, S])).

