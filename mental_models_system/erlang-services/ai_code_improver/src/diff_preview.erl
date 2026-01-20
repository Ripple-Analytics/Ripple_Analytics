-module(diff_preview).
-behaviour(gen_server).

-export([start_link/0, create_diff/2, get_pending_diffs/0, approve_diff/1, reject_diff/1]).
-export([get_diff/1, clear_pending/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-record(state, {
    pending_diffs :: map(),  % DiffId -> DiffRecord
    next_id :: non_neg_integer()
}).

-record(diff_record, {
    id :: binary(),
    file_path :: string(),
    original_content :: binary(),
    new_content :: binary(),
    diff_text :: binary(),
    description :: binary(),
    created_at :: erlang:timestamp(),
    status :: pending | approved | rejected | applied
}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

create_diff(FilePath, NewContent) ->
    gen_server:call(?MODULE, {create_diff, FilePath, NewContent}, 30000).

get_pending_diffs() ->
    gen_server:call(?MODULE, get_pending_diffs, 5000).

get_diff(DiffId) ->
    gen_server:call(?MODULE, {get_diff, DiffId}, 5000).

approve_diff(DiffId) ->
    gen_server:call(?MODULE, {approve_diff, DiffId}, 30000).

reject_diff(DiffId) ->
    gen_server:call(?MODULE, {reject_diff, DiffId}, 5000).

clear_pending() ->
    gen_server:call(?MODULE, clear_pending, 5000).

init([]) ->
    {ok, #state{
        pending_diffs = #{},
        next_id = 1
    }}.

handle_call({create_diff, FilePath, NewContent}, _From, State) ->
    case file:read_file(FilePath) of
        {ok, OriginalContent} ->
            DiffId = generate_diff_id(State#state.next_id),
            DiffText = generate_diff_text(FilePath, OriginalContent, NewContent),
            
            DiffRecord = #diff_record{
                id = DiffId,
                file_path = FilePath,
                original_content = OriginalContent,
                new_content = NewContent,
                diff_text = DiffText,
                description = <<"AI-generated improvement">>,
                created_at = erlang:timestamp(),
                status = pending
            },
            
            NewPending = maps:put(DiffId, DiffRecord, State#state.pending_diffs),
            NewState = State#state{
                pending_diffs = NewPending,
                next_id = State#state.next_id + 1
            },
            
            {reply, {ok, #{
                <<"id">> => DiffId,
                <<"file">> => list_to_binary(FilePath),
                <<"diff">> => DiffText,
                <<"status">> => <<"pending">>
            }}, NewState};
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;

handle_call(get_pending_diffs, _From, State) ->
    Diffs = maps:fold(fun(_Id, Diff, Acc) ->
        case Diff#diff_record.status of
            pending ->
                [#{
                    <<"id">> => Diff#diff_record.id,
                    <<"file">> => list_to_binary(Diff#diff_record.file_path),
                    <<"description">> => Diff#diff_record.description,
                    <<"created_at">> => format_timestamp(Diff#diff_record.created_at),
                    <<"status">> => atom_to_binary(Diff#diff_record.status, utf8)
                } | Acc];
            _ ->
                Acc
        end
    end, [], State#state.pending_diffs),
    {reply, {ok, Diffs}, State};

handle_call({get_diff, DiffId}, _From, State) ->
    case maps:get(DiffId, State#state.pending_diffs, undefined) of
        undefined ->
            {reply, {error, not_found}, State};
        Diff ->
            {reply, {ok, #{
                <<"id">> => Diff#diff_record.id,
                <<"file">> => list_to_binary(Diff#diff_record.file_path),
                <<"diff">> => Diff#diff_record.diff_text,
                <<"description">> => Diff#diff_record.description,
                <<"created_at">> => format_timestamp(Diff#diff_record.created_at),
                <<"status">> => atom_to_binary(Diff#diff_record.status, utf8)
            }}, State}
    end;

handle_call({approve_diff, DiffId}, _From, State) ->
    case maps:get(DiffId, State#state.pending_diffs, undefined) of
        undefined ->
            {reply, {error, not_found}, State};
        Diff when Diff#diff_record.status =:= pending ->
            case file:write_file(Diff#diff_record.file_path, Diff#diff_record.new_content) of
                ok ->
                    UpdatedDiff = Diff#diff_record{status = applied},
                    NewPending = maps:put(DiffId, UpdatedDiff, State#state.pending_diffs),
                    {reply, {ok, applied}, State#state{pending_diffs = NewPending}};
                {error, Reason} ->
                    {reply, {error, Reason}, State}
            end;
        Diff ->
            {reply, {error, {invalid_status, Diff#diff_record.status}}, State}
    end;

handle_call({reject_diff, DiffId}, _From, State) ->
    case maps:get(DiffId, State#state.pending_diffs, undefined) of
        undefined ->
            {reply, {error, not_found}, State};
        Diff when Diff#diff_record.status =:= pending ->
            UpdatedDiff = Diff#diff_record{status = rejected},
            NewPending = maps:put(DiffId, UpdatedDiff, State#state.pending_diffs),
            {reply, {ok, rejected}, State#state{pending_diffs = NewPending}};
        Diff ->
            {reply, {error, {invalid_status, Diff#diff_record.status}}, State}
    end;

handle_call(clear_pending, _From, State) ->
    PendingOnly = maps:filter(fun(_Id, Diff) ->
        Diff#diff_record.status =/= pending
    end, State#state.pending_diffs),
    {reply, ok, State#state{pending_diffs = PendingOnly}}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

generate_diff_id(N) ->
    list_to_binary(io_lib:format("diff-~6..0B", [N])).

generate_diff_text(FilePath, OriginalContent, NewContent) ->
    OrigLines = binary:split(OriginalContent, <<"\n">>, [global]),
    NewLines = binary:split(NewContent, <<"\n">>, [global]),
    
    Header = io_lib:format("--- a/~s~n+++ b/~s~n", [FilePath, FilePath]),
    
    DiffLines = compute_simple_diff(OrigLines, NewLines, 1, []),
    
    iolist_to_binary([Header | DiffLines]).

compute_simple_diff([], [], _LineNum, Acc) ->
    lists:reverse(Acc);
compute_simple_diff([O|OrigRest], [N|NewRest], LineNum, Acc) when O =:= N ->
    compute_simple_diff(OrigRest, NewRest, LineNum + 1, Acc);
compute_simple_diff([O|OrigRest], [N|NewRest], LineNum, Acc) ->
    Line = io_lib:format("@@ -~p +~p @@~n-~s~n+~s~n", [LineNum, LineNum, O, N]),
    compute_simple_diff(OrigRest, NewRest, LineNum + 1, [Line | Acc]);
compute_simple_diff([O|OrigRest], [], LineNum, Acc) ->
    Line = io_lib:format("@@ -~p @@~n-~s~n", [LineNum, O]),
    compute_simple_diff(OrigRest, [], LineNum + 1, [Line | Acc]);
compute_simple_diff([], [N|NewRest], LineNum, Acc) ->
    Line = io_lib:format("@@ +~p @@~n+~s~n", [LineNum, N]),
    compute_simple_diff([], NewRest, LineNum + 1, [Line | Acc]).

format_timestamp(Timestamp) ->
    {{Y, M, D}, {H, Mi, S}} = calendar:now_to_datetime(Timestamp),
    list_to_binary(io_lib:format("~4..0B-~2..0B-~2..0BT~2..0B:~2..0B:~2..0BZ", 
                                  [Y, M, D, H, Mi, S])).
