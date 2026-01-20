%%%-------------------------------------------------------------------
%%% @doc Google Drive Worker
%%% Handles downloading from and uploading to Google Drive.
%%% @end
%%%-------------------------------------------------------------------
-module(gdrive_worker).
-behaviour(gen_server).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).
-export([get_status/0, check_available/0, fetch_latest/0, upload_backup/1]).

-record(state, {
    status = idle,
    available = false,
    last_check = undefined,
    last_download = undefined,
    last_upload = undefined,
    backup_url = undefined,
    local_path = "/repo/gdrive",
    error = undefined
}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

get_status() -> gen_server:call(?MODULE, get_status).
check_available() -> gen_server:call(?MODULE, check_available, 30000).
fetch_latest() -> gen_server:call(?MODULE, fetch_latest, 120000).
upload_backup(SourcePath) -> gen_server:call(?MODULE, {upload_backup, SourcePath}, 300000).

init([]) ->
    io:format("[GDRIVE-WORKER] Starting~n"),
    BackupUrl = os:getenv("GDRIVE_BACKUP_URL", undefined),
    Available = BackupUrl =/= undefined andalso BackupUrl =/= false,
    os:cmd("mkdir -p /repo/gdrive"),
    {ok, #state{backup_url = BackupUrl, available = Available}}.

handle_call(get_status, _From, State) ->
    Status = #{
        source => <<"gdrive">>,
        status => State#state.status,
        available => State#state.available,
        last_check => State#state.last_check,
        last_download => State#state.last_download,
        last_upload => State#state.last_upload,
        configured => State#state.backup_url =/= undefined,
        error => State#state.error
    },
    {reply, {ok, Status}, State};

handle_call(check_available, _From, State) ->
    Available = State#state.backup_url =/= undefined andalso State#state.backup_url =/= false,
    Now = erlang:timestamp(),
    {reply, {ok, #{available => Available}}, State#state{last_check = Now, available = Available}};

handle_call(fetch_latest, _From, State) ->
    case State#state.backup_url of
        undefined ->
            {reply, {error, <<"No Google Drive URL configured">>}, State#state{status = error}};
        false ->
            {reply, {error, <<"No Google Drive URL configured">>}, State#state{status = error}};
        Url ->
            NewState = do_fetch(Url, State),
            Result = case NewState#state.status of
                idle -> {ok, #{path => list_to_binary(NewState#state.local_path)}};
                error -> {error, NewState#state.error}
            end,
            {reply, Result, NewState}
    end;

handle_call({upload_backup, SourcePath}, _From, State) ->
    NewState = do_upload(SourcePath, State),
    Result = case NewState#state.status of
        idle -> {ok, #{message => <<"Backup uploaded">>}};
        error -> {error, NewState#state.error}
    end,
    {reply, Result, NewState};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

%% Internal functions

do_fetch(Url, State) ->
    io:format("[GDRIVE-WORKER] Fetching from: ~s~n", [Url]),
    LocalPath = State#state.local_path,
    ZipPath = "/tmp/gdrive_backup.zip",
    
    %% Convert Google Drive view URL to direct download
    DownloadUrl = convert_to_download_url(Url),
    
    Cmd = "curl -L -o " ++ ZipPath ++ " '" ++ DownloadUrl ++ "' 2>&1",
    CurlResult = os:cmd(Cmd),
    
    case filelib:file_size(ZipPath) > 1000 of
        true ->
            %% Extract
            os:cmd("rm -rf " ++ LocalPath ++ "/*"),
            ExtractCmd = "unzip -o " ++ ZipPath ++ " -d " ++ LocalPath ++ " 2>&1",
            os:cmd(ExtractCmd),
            os:cmd("rm " ++ ZipPath),
            Now = erlang:timestamp(),
            io:format("[GDRIVE-WORKER] Fetch successful~n"),
            State#state{status = idle, last_download = Now, error = undefined};
        false ->
            io:format("[GDRIVE-WORKER] Fetch failed: ~s~n", [CurlResult]),
            State#state{status = error, error = <<"Download failed or file too small">>}
    end.

do_upload(SourcePath, State) ->
    io:format("[GDRIVE-WORKER] Upload requested from: ~s~n", [SourcePath]),
    %% For upload, we need rclone or gcloud configured
    %% This is a placeholder - actual implementation depends on auth method
    case os:getenv("GDRIVE_RCLONE_REMOTE") of
        false ->
            State#state{status = error, error = <<"No rclone remote configured for upload">>};
        Remote ->
            ZipPath = "/tmp/backup_upload.zip",
            os:cmd("cd " ++ SourcePath ++ " && zip -r " ++ ZipPath ++ " . -x '*.git*' 2>&1"),
            UploadCmd = "rclone copy " ++ ZipPath ++ " " ++ Remote ++ ":Mental_Models_Backup/ 2>&1",
            os:cmd(UploadCmd),
            os:cmd("rm " ++ ZipPath),
            Now = erlang:timestamp(),
            State#state{status = idle, last_upload = Now, error = undefined}
    end.

convert_to_download_url(Url) ->
    %% Convert Google Drive share URL to direct download
    case string:find(Url, "drive.google.com/file/d/") of
        nomatch -> Url;
        _ ->
            %% Extract file ID
            Parts = string:split(Url, "/d/", all),
            case length(Parts) >= 2 of
                true ->
                    IdPart = lists:nth(2, Parts),
                    FileId = hd(string:split(IdPart, "/")),
                    "https://drive.google.com/uc?export=download&id=" ++ FileId;
                false -> Url
            end
    end.
