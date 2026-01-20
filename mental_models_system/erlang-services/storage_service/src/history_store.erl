%%%-------------------------------------------------------------------
%%% @doc History Store - DETS-backed analysis history storage
%%%-------------------------------------------------------------------
-module(history_store).
-behaviour(gen_server).

-export([start_link/0, save_analysis/1, get_analysis/1, list_analyses/0, 
         list_analyses/1, delete_analysis/1, get_stats/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-define(TABLE, analysis_history).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

save_analysis(Analysis) ->
    gen_server:call(?MODULE, {save, Analysis}).

get_analysis(Id) ->
    gen_server:call(?MODULE, {get, Id}).

list_analyses() ->
    list_analyses(#{}).

list_analyses(Opts) ->
    gen_server:call(?MODULE, {list, Opts}).

delete_analysis(Id) ->
    gen_server:call(?MODULE, {delete, Id}).

get_stats() ->
    gen_server:call(?MODULE, get_stats).

init([]) ->
    DataDir = application:get_env(storage_service, data_dir, "/data"),
    filelib:ensure_dir(DataDir ++ "/"),
    FilePath = DataDir ++ "/analysis_history.dets",
    {ok, ?TABLE} = dets:open_file(?TABLE, [{file, FilePath}, {type, set}]),
    {ok, #{table => ?TABLE}}.

handle_call({save, Analysis}, _From, State) ->
    Id = generate_id(),
    Timestamp = erlang:system_time(second),
    
    %% Extract key fields
    InputText = maps:get(<<"input_text">>, Analysis, <<>>),
    AnalysisType = maps:get(<<"type">>, Analysis, <<"full">>),
    Models = maps:get(<<"models">>, Analysis, []),
    Biases = maps:get(<<"biases">>, Analysis, []),
    
    Record = #{
        <<"id">> => Id,
        <<"timestamp">> => Timestamp,
        <<"input_text">> => truncate_text(InputText, 500),
        <<"type">> => AnalysisType,
        <<"models">> => Models,
        <<"biases">> => Biases,
        <<"model_count">> => length(Models),
        <<"bias_count">> => length(Biases)
    },
    
    ok = dets:insert(?TABLE, {Id, Record}),
    {reply, {ok, Record}, State};

handle_call({get, Id}, _From, State) ->
    Result = case dets:lookup(?TABLE, Id) of
        [{Id, Record}] -> {ok, Record};
        [] -> {error, not_found}
    end,
    {reply, Result, State};

handle_call({list, Opts}, _From, State) ->
    Limit = maps:get(limit, Opts, 50),
    Type = maps:get(type, Opts, all),
    
    All = dets:foldl(fun({_Id, Record}, Acc) ->
        case Type of
            all -> [Record | Acc];
            _ -> 
                RecordType = maps:get(<<"type">>, Record, <<>>),
                case RecordType =:= Type of
                    true -> [Record | Acc];
                    false -> Acc
                end
        end
    end, [], ?TABLE),
    
    %% Sort by timestamp descending
    Sorted = lists:reverse(lists:keysort(1, 
        [{maps:get(<<"timestamp">>, R), R} || R <- All])),
    
    %% Apply limit
    Limited = lists:sublist([R || {_, R} <- Sorted], Limit),
    
    {reply, {ok, Limited}, State};

handle_call({delete, Id}, _From, State) ->
    ok = dets:delete(?TABLE, Id),
    {reply, ok, State};

handle_call(get_stats, _From, State) ->
    Stats = dets:foldl(fun({_Id, Record}, Acc) ->
        Type = maps:get(<<"type">>, Record, <<"unknown">>),
        ModelCount = maps:get(<<"model_count">>, Record, 0),
        BiasCount = maps:get(<<"bias_count">>, Record, 0),
        
        #{
            total => maps:get(total, Acc, 0) + 1,
            by_type => maps:update_with(Type, fun(V) -> V + 1 end, 1, 
                maps:get(by_type, Acc, #{})),
            total_models_detected => maps:get(total_models_detected, Acc, 0) + ModelCount,
            total_biases_detected => maps:get(total_biases_detected, Acc, 0) + BiasCount
        }
    end, #{total => 0, by_type => #{}, total_models_detected => 0, total_biases_detected => 0}, ?TABLE),
    {reply, {ok, Stats}, State};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) -> {noreply, State}.
handle_info(_Info, State) -> {noreply, State}.

terminate(_Reason, _State) ->
    dets:close(?TABLE),
    ok.

%% Internal functions
generate_id() ->
    Timestamp = erlang:system_time(microsecond),
    Random = rand:uniform(999999),
    list_to_binary(io_lib:format("~p-~6..0B", [Timestamp, Random])).

truncate_text(Text, MaxLen) when byte_size(Text) > MaxLen ->
    <<Truncated:MaxLen/binary, _/binary>> = Text,
    <<Truncated/binary, "...">>;
truncate_text(Text, _MaxLen) ->
    Text.
