-module(rollback_manager).
%% Helper modules: rollback_manager_part2
-behaviour(gen_server).

-export([start_link/0, save_backup/1, rollback/1, get_backups/0, get_backup/1]).
-export([cleanup_old_backups/0, get_backup_count/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-record(state, {
    backups :: map(),  % BackupId -> BackupRecord
    backup_dir :: string(),
    max_backups :: non_neg_integer(),
    next_id :: non_neg_integer()
}).

-record(backup_record, {
    id :: binary(),
    file_path :: string(),
    original_content :: binary(),
    backup_file :: string(),
    created_at :: erlang:timestamp(),
    description :: binary(),
    rolled_back :: boolean()
}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

save_backup(FilePath) ->
    gen_server:call(?MODULE, {save_backup, FilePath}, 30000).

rollback(BackupId) ->
    gen_server:call(?MODULE, {rollback, BackupId}, 30000).

get_backups() ->
    gen_server:call(?MODULE, get_backups, 5000).

get_backup(BackupId) ->
    gen_server:call(?MODULE, {get_backup, BackupId}, 5000).

cleanup_old_backups() ->
    gen_server:call(?MODULE, cleanup_old_backups, 30000).

get_backup_count() ->
    gen_server:call(?MODULE, get_backup_count, 5000).

init([]) ->
    BackupDir = "/data/backups",
    filelib:ensure_dir(BackupDir ++ "/"),
    MaxBackups = application:get_env(ai_code_improver, max_backups, 100),
    
    Backups = load_backup_index(BackupDir),
    NextId = maps:size(Backups) + 1,
    
    {ok, #state{
        backups = Backups,
        backup_dir = BackupDir,
        max_backups = MaxBackups,
        next_id = NextId
    }}.

handle_call({save_backup, FilePath}, _From, State) ->
    case file:read_file(FilePath) of
        {ok, Content} ->
            BackupId = generate_backup_id(State#state.next_id),
            BackupFile = filename:join(State#state.backup_dir, 
                                        binary_to_list(BackupId) ++ ".bak"),
            
            case file:write_file(BackupFile, Content) of
                ok ->
                    BackupRecord = #backup_record{
                        id = BackupId,
                        file_path = FilePath,
                        original_content = Content,
                        backup_file = BackupFile,
                        created_at = erlang:timestamp(),
                        description = list_to_binary("Backup of " ++ FilePath),
                        rolled_back = false
                    },
                    
                    NewBackups = maps:put(BackupId, BackupRecord, State#state.backups),
                    save_backup_index(State#state.backup_dir, NewBackups),
                    
                    NewState = State#state{
                        backups = NewBackups,
                        next_id = State#state.next_id + 1
                    },
                    
                    {reply, {ok, #{
                        <<"id">> => BackupId,
                        <<"file">> => list_to_binary(FilePath),
                        <<"backup_file">> => list_to_binary(BackupFile)
                    }}, NewState};
                {error, Reason} ->
                    {reply, {error, Reason}, State}
            end;
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;

handle_call({rollback, BackupId}, _From, State) ->
    case maps:get(BackupId, State#state.backups, undefined) of
        undefined ->
            {reply, {error, not_found}, State};
        Backup ->
            case file:read_file(Backup#backup_record.backup_file) of
                {ok, Content} ->
                    case file:write_file(Backup#backup_record.file_path, Content) of
                        ok ->
                            UpdatedBackup = Backup#backup_record{rolled_back = true},
                            NewBackups = maps:put(BackupId, UpdatedBackup, State#state.backups),
                            save_backup_index(State#state.backup_dir, NewBackups),
                            
                            {reply, {ok, #{
                                <<"id">> => BackupId,
                                <<"file">> => list_to_binary(Backup#backup_record.file_path),
                                <<"status">> => <<"rolled_back">>
                            }}, State#state{backups = NewBackups}};
                        {error, Reason} ->
                            {reply, {error, Reason}, State}
                    end;
                {error, Reason} ->
                    {reply, {error, Reason}, State}
            end
    end;

handle_call(get_backups, _From, State) ->
    Backups = maps:fold(fun(_Id, Backup, Acc) ->
        [#{
            <<"id">> => Backup#backup_record.id,
            <<"file">> => list_to_binary(Backup#backup_record.file_path),
            <<"created_at">> => format_timestamp(Backup#backup_record.created_at),
            <<"rolled_back">> => Backup#backup_record.rolled_back
        } | Acc]
    end, [], State#state.backups),
    SortedBackups = lists:sort(fun(A, B) ->
        maps:get(<<"created_at">>, A) > maps:get(<<"created_at">>, B)
    end, Backups),
    {reply, {ok, SortedBackups}, State};
