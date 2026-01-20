%%%-------------------------------------------------------------------
%%% @doc Google Drive Client
%%% 
%%% Handles communication with Google Drive API for backup operations.
%%% Supports both direct URL downloads and Google Drive API.
%%% @end
%%%-------------------------------------------------------------------
-module(gdrive_client).
%% Helper modules: gdrive_client_part2
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
