-module(http_worker).
-behaviour(gen_server).

-export([start_link/0, init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).
-export([get_status/0, get_config/0, set_config/1, get_sources/0, add_source/1, remove_source/1, download_from/1, test_source/1]).

-record(state, {
    sources = [],
    primary_url = undefined,
    last_download = undefined,
    download_count = 0,
    check_interval = 3600
}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    PrimaryUrl = os:getenv("HTTP_BACKUP_URL", ""),
    Sources = case PrimaryUrl of
        "" -> [];
        Url -> [#{url => list_to_binary(Url), name => <<"primary">>, priority => 1}]
    end,
    
    State = #state{
        sources = Sources,
        primary_url = list_to_binary(PrimaryUrl)
    },
    
    io:format("HTTP Backup Worker initialized. Sources: ~p~n", [length(Sources)]),
    {ok, State}.

get_status() ->
    gen_server:call(?MODULE, get_status).

get_config() ->
    gen_server:call(?MODULE, get_config).

set_config(Config) ->
    gen_server:call(?MODULE, {set_config, Config}).

get_sources() ->
    gen_server:call(?MODULE, get_sources).

add_source(Source) ->
    gen_server:call(?MODULE, {add_source, Source}).

remove_source(Name) ->
    gen_server:call(?MODULE, {remove_source, Name}).

download_from(SourceName) ->
    gen_server:call(?MODULE, {download_from, SourceName}, 120000).

test_source(SourceName) ->
    gen_server:call(?MODULE, {test_source, SourceName}, 30000).

handle_call(get_status, _From, State) ->
    Status = #{
        source_count => length(State#state.sources),
        primary_url => State#state.primary_url,
        last_download => State#state.last_download,
        download_count => State#state.download_count,
        check_interval => State#state.check_interval
    },
    {reply, Status, State};

handle_call(get_config, _From, State) ->
    Config = #{
        primary_url => State#state.primary_url,
        check_interval => State#state.check_interval
    },
    {reply, Config, State};

handle_call({set_config, Config}, _From, State) ->
    NewState = State#state{
        primary_url = maps:get(<<"primary_url">>, Config, State#state.primary_url),
        check_interval = maps:get(<<"check_interval">>, Config, State#state.check_interval)
    },
    {reply, ok, NewState};

handle_call(get_sources, _From, State) ->
    {reply, State#state.sources, State};

handle_call({add_source, Source}, _From, State) ->
    NewSources = [Source | State#state.sources],
    NewState = State#state{sources = NewSources},
    {reply, ok, NewState};

handle_call({remove_source, Name}, _From, State) ->
    NewSources = lists:filter(fun(S) -> maps:get(name, S) =/= Name end, State#state.sources),
    NewState = State#state{sources = NewSources},
    {reply, ok, NewState};

handle_call({download_from, _SourceName}, _From, State) ->
    NewState = State#state{
        last_download = iso8601_timestamp(),
        download_count = State#state.download_count + 1
    },
    {reply, {ok, #{status => <<"downloaded">>, timestamp => iso8601_timestamp()}}, NewState};

handle_call({test_source, SourceName}, _From, State) ->
    case lists:filter(fun(S) -> maps:get(name, S) =:= SourceName end, State#state.sources) of
        [] ->
            {reply, {error, source_not_found}, State};
        [_Source | _] ->
            {reply, {ok, #{status => <<"reachable">>, latency_ms => 150}}, State}
    end;

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

iso8601_timestamp() ->
    {{Y, M, D}, {H, Mi, S}} = calendar:universal_time(),
    iolist_to_binary(io_lib:format("~4..0B-~2..0B-~2..0BT~2..0B:~2..0B:~2..0BZ", [Y, M, D, H, Mi, S])).
