%%%-------------------------------------------------------------------
%%% @doc rollback_manager Helper Module - Part 2
%%% @end
%%%-------------------------------------------------------------------
-module(rollback_manager_part2).

-export([handle_call/4, handle_call/3, handle_call/3, handle_cast/2, handle_info/2, terminate/2, generate_backup_id/1, format_timestamp/1, load_backup_index/1, save_backup_index/2]).

handle_call({get_backup, BackupId}, _From, State) ->
    case maps:get(BackupId, State#state.backups, undefined) of
        undefined ->
            {reply, {error, not_found}, State};
        Backup ->
            {reply, {ok, #{
                <<"id">> => Backup#backup_record.id,
                <<"file">> => list_to_binary(Backup#backup_record.file_path),
                <<"backup_file">> => list_to_binary(Backup#backup_record.backup_file),
                <<"created_at">> => format_timestamp(Backup#backup_record.created_at),
                <<"description">> => Backup#backup_record.description,
                <<"rolled_back">> => Backup#backup_record.rolled_back
            }}, State}
    end;

handle_call(cleanup_old_backups, _From, State) ->
    BackupList = maps:to_list(State#state.backups),
    SortedBackups = lists:sort(fun({_, A}, {_, B}) ->
        A#backup_record.created_at < B#backup_record.created_at
    end, BackupList),
    
    ToRemove = max(0, length(SortedBackups) - State#state.max_backups),
    {OldBackups, _} = lists:split(ToRemove, SortedBackups),
    
    lists:foreach(fun({_Id, Backup}) ->
        file:delete(Backup#backup_record.backup_file)
    end, OldBackups),
    
    NewBackups = lists:foldl(fun({Id, _}, Acc) ->
        maps:remove(Id, Acc)
    end, State#state.backups, OldBackups),
    
    save_backup_index(State#state.backup_dir, NewBackups),
    
    {reply, {ok, #{<<"removed">> => ToRemove}}, State#state{backups = NewBackups}};

handle_call(get_backup_count, _From, State) ->
    {reply, {ok, maps:size(State#state.backups)}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

generate_backup_id(N) ->
    {{Y, M, D}, {H, Mi, S}} = calendar:local_time(),
    list_to_binary(io_lib:format("backup-~4..0B~2..0B~2..0B-~2..0B~2..0B~2..0B-~6..0B", 
                                  [Y, M, D, H, Mi, S, N])).

format_timestamp(Timestamp) ->
    {{Y, M, D}, {H, Mi, S}} = calendar:now_to_datetime(Timestamp),
    list_to_binary(io_lib:format("~4..0B-~2..0B-~2..0BT~2..0B:~2..0B:~2..0BZ", 
                                  [Y, M, D, H, Mi, S])).

load_backup_index(BackupDir) ->
    IndexFile = filename:join(BackupDir, "index.json"),
    case file:read_file(IndexFile) of
        {ok, Content} ->
            try
                Data = jsx:decode(Content, [return_maps]),
                maps:fold(fun(Id, Info, Acc) ->
                    Backup = #backup_record{
                        id = Id,
                        file_path = binary_to_list(maps:get(<<"file_path">>, Info)),
                        original_content = <<>>,
                        backup_file = binary_to_list(maps:get(<<"backup_file">>, Info)),
                        created_at = erlang:timestamp(),
                        description = maps:get(<<"description">>, Info, <<>>),
                        rolled_back = maps:get(<<"rolled_back">>, Info, false)
                    },
                    maps:put(Id, Backup, Acc)
                end, #{}, Data)
            catch
                _:_ -> #{}
            end;
        {error, _} ->
            #{}
    end.

save_backup_index(BackupDir, Backups) ->
    IndexFile = filename:join(BackupDir, "index.json"),
    Data = maps:fold(fun(Id, Backup, Acc) ->
        maps:put(Id, #{
            <<"file_path">> => list_to_binary(Backup#backup_record.file_path),
            <<"backup_file">> => list_to_binary(Backup#backup_record.backup_file),
            <<"description">> => Backup#backup_record.description,
            <<"rolled_back">> => Backup#backup_record.rolled_back
        }, Acc)
    end, #{}, Backups),
    file:write_file(IndexFile, jsx:encode(Data)).

