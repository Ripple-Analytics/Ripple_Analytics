%%%-------------------------------------------------------------------
%%% @doc Google Drive Client
%%% 
%%% Handles communication with Google Drive API for backup operations.
%%% Supports both direct URL downloads and Google Drive API.
%%% @end
%%%-------------------------------------------------------------------
-module(gdrive_client).
-behaviour(gen_server).

-export([start_link/0, download/1, upload/2, list_backups/0, get_status/0, set_config/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-define(SERVER, ?MODULE).
-define(TIMEOUT, 120000).
-define(CONFIG_FILE, "/data/gdrive_config.json").
-define(STATUS_FILE, "/data/gdrive_status.json").

-record(state, {
    gdrive_url :: binary(),
    folder_id :: binary(),
    api_key :: binary(),
    last_sync :: undefined | calendar:datetime(),
    status :: idle | downloading | uploading | error,
    error_message :: undefined | binary()
}).

%%====================================================================
%% API
%%====================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

download(FileId) ->
    gen_server:call(?SERVER, {download, FileId}, ?TIMEOUT).

upload(FilePath, FileName) ->
    gen_server:call(?SERVER, {upload, FilePath, FileName}, ?TIMEOUT).

list_backups() ->
    gen_server:call(?SERVER, list_backups, ?TIMEOUT).

get_status() ->
    gen_server:call(?SERVER, get_status).

set_config(Config) ->
    gen_server:call(?SERVER, {set_config, Config}).

%%====================================================================
%% gen_server callbacks
%%====================================================================

init([]) ->
    %% Load config from file
    Config = load_config(),
    
    State = #state{
        gdrive_url = maps:get(<<"gdrive_url">>, Config, <<>>),
        folder_id = maps:get(<<"folder_id">>, Config, <<>>),
        api_key = maps:get(<<"api_key">>, Config, <<>>),
        last_sync = undefined,
        status = idle,
        error_message = undefined
    },
    
    write_status(State),
    {ok, State}.

handle_call({download, FileIdOrUrl}, _From, State) ->
    NewState = State#state{status = downloading},
    write_status(NewState),
    
    Result = do_download(FileIdOrUrl, State),
    
    FinalState = case Result of
        {ok, _} -> 
            NewState#state{status = idle, last_sync = calendar:local_time(), error_message = undefined};
        {error, Reason} -> 
            NewState#state{status = error, error_message = list_to_binary(io_lib:format("~p", [Reason]))}
    end,
    write_status(FinalState),
    {reply, Result, FinalState};

handle_call({upload, FilePath, FileName}, _From, State) ->
    NewState = State#state{status = uploading},
    write_status(NewState),
    
    Result = do_upload(FilePath, FileName, State),
    
    FinalState = case Result of
        {ok, _} -> 
            NewState#state{status = idle, last_sync = calendar:local_time(), error_message = undefined};
        {error, Reason} -> 
            NewState#state{status = error, error_message = list_to_binary(io_lib:format("~p", [Reason]))}
    end,
    write_status(FinalState),
    {reply, Result, FinalState};

handle_call(list_backups, _From, State) ->
    Result = do_list_backups(State),
    {reply, Result, State};

handle_call(get_status, _From, State) ->
    Status = #{
        <<"status">> => atom_to_binary(State#state.status),
        <<"gdrive_configured">> => byte_size(State#state.gdrive_url) > 0 orelse byte_size(State#state.folder_id) > 0,
        <<"last_sync">> => format_datetime(State#state.last_sync),
        <<"error_message">> => State#state.error_message
    },
    {reply, {ok, Status}, State};

handle_call({set_config, Config}, _From, State) ->
    NewState = State#state{
        gdrive_url = maps:get(<<"gdrive_url">>, Config, State#state.gdrive_url),
        folder_id = maps:get(<<"folder_id">>, Config, State#state.folder_id),
        api_key = maps:get(<<"api_key">>, Config, State#state.api_key)
    },
    save_config(Config),
    write_status(NewState),
    {reply, ok, NewState};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================

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
