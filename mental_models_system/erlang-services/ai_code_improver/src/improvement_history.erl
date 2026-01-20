-module(improvement_history).
-behaviour(gen_server).

-export([start_link/0, record/1, get_history/0, get_history/1, clear_history/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-record(state, {
    history :: list(),
    history_file :: string()
}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

record(Improvements) when is_list(Improvements) ->
    gen_server:cast(?MODULE, {record, Improvements});
record(Improvement) when is_map(Improvement) ->
    gen_server:cast(?MODULE, {record, [Improvement]}).

get_history() ->
    gen_server:call(?MODULE, get_history).

get_history(Limit) ->
    gen_server:call(?MODULE, {get_history, Limit}).

clear_history() ->
    gen_server:call(?MODULE, clear_history).

init([]) ->
    HistoryFile = "/data/improvement_history.json",
    History = load_history(HistoryFile),
    {ok, #state{
        history = History,
        history_file = HistoryFile
    }}.

handle_call(get_history, _From, State) ->
    {reply, {ok, State#state.history}, State};

handle_call({get_history, Limit}, _From, State) ->
    Limited = lists:sublist(State#state.history, Limit),
    {reply, {ok, Limited}, State};

handle_call(clear_history, _From, State) ->
    save_history(State#state.history_file, []),
    {reply, ok, State#state{history = []}}.

handle_cast({record, Improvements}, State) ->
    NewHistory = Improvements ++ State#state.history,
    TrimmedHistory = lists:sublist(NewHistory, 1000),
    save_history(State#state.history_file, TrimmedHistory),
    io:format("[AI Improver] Recorded ~p improvements (total: ~p)~n", 
              [length(Improvements), length(TrimmedHistory)]),
    {noreply, State#state{history = TrimmedHistory}}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

load_history(FilePath) ->
    case file:read_file(FilePath) of
        {ok, Content} ->
            try
                jsx:decode(Content, [return_maps])
            catch
                _:_ -> []
            end;
        {error, _} ->
            []
    end.

save_history(FilePath, History) ->
    filelib:ensure_dir(FilePath),
    Content = jsx:encode(History),
    file:write_file(FilePath, Content).
