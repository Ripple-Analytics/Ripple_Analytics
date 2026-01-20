%%%-------------------------------------------------------------------
%%% @doc Document Store - DETS-backed persistent storage
%%%-------------------------------------------------------------------
-module(document_store).
-behaviour(gen_server).

-export([start_link/0, store/2, get/1, delete/1, search/1, list_all/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-define(TABLE, documents).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

store(Id, Document) ->
    gen_server:call(?MODULE, {store, Id, Document}).

get(Id) ->
    gen_server:call(?MODULE, {get, Id}).

delete(Id) ->
    gen_server:call(?MODULE, {delete, Id}).

search(Query) ->
    gen_server:call(?MODULE, {search, Query}).

list_all() ->
    gen_server:call(?MODULE, list_all).

init([]) ->
    DataDir = application:get_env(storage_service, data_dir, "/data"),
    filelib:ensure_dir(DataDir ++ "/"),
    FilePath = DataDir ++ "/documents.dets",
    {ok, ?TABLE} = dets:open_file(?TABLE, [{file, FilePath}, {type, set}]),
    {ok, #{table => ?TABLE}}.

handle_call({store, Id, Document}, _From, State) ->
    Timestamp = erlang:system_time(second),
    Doc = Document#{<<"id">> => Id, <<"created_at">> => Timestamp, <<"updated_at">> => Timestamp},
    ok = dets:insert(?TABLE, {Id, Doc}),
    {reply, {ok, Doc}, State};

handle_call({get, Id}, _From, State) ->
    Result = case dets:lookup(?TABLE, Id) of
        [{Id, Doc}] -> {ok, Doc};
        [] -> {error, not_found}
    end,
    {reply, Result, State};

handle_call({delete, Id}, _From, State) ->
    ok = dets:delete(?TABLE, Id),
    {reply, ok, State};

handle_call({search, Query}, _From, State) ->
    QueryLower = string:lowercase(binary_to_list(Query)),
    All = dets:foldl(fun({_Id, Doc}, Acc) ->
        Content = maps:get(<<"content">>, Doc, <<>>),
        Title = maps:get(<<"title">>, Doc, <<>>),
        ContentLower = string:lowercase(binary_to_list(Content)),
        TitleLower = string:lowercase(binary_to_list(Title)),
        case string:find(ContentLower, QueryLower) =/= nomatch orelse
             string:find(TitleLower, QueryLower) =/= nomatch of
            true -> [Doc | Acc];
            false -> Acc
        end
    end, [], ?TABLE),
    {reply, {ok, All}, State};

handle_call(list_all, _From, State) ->
    All = dets:foldl(fun({_Id, Doc}, Acc) -> [Doc | Acc] end, [], ?TABLE),
    {reply, {ok, All}, State};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) -> {noreply, State}.
handle_info(_Info, State) -> {noreply, State}.

terminate(_Reason, _State) ->
    dets:close(?TABLE),
    ok.
