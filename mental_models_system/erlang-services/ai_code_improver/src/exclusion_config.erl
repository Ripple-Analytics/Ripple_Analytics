-module(exclusion_config).
-behaviour(gen_server).

-export([start_link/0, is_excluded/1, add_pattern/1, remove_pattern/1]).
-export([get_patterns/0, set_patterns/1, reset_to_defaults/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-record(state, {
    patterns :: list(),
    config_file :: string()
}).

-define(DEFAULT_PATTERNS, [
    "*.beam",
    "*.o",
    "*.pyc",
    "_build/*",
    "deps/*",
    "node_modules/*",
    ".git/*",
    "*.log",
    "*.bak",
    "*.tmp",
    "rebar.lock",
    "package-lock.json",
    "*.crashdump",
    "priv/static/*",
    "test/*_SUITE.erl",
    ".eunit/*",
    ".rebar3/*"
]).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

is_excluded(FilePath) ->
    gen_server:call(?MODULE, {is_excluded, FilePath}, 5000).

add_pattern(Pattern) ->
    gen_server:call(?MODULE, {add_pattern, Pattern}, 5000).

remove_pattern(Pattern) ->
    gen_server:call(?MODULE, {remove_pattern, Pattern}, 5000).

get_patterns() ->
    gen_server:call(?MODULE, get_patterns, 5000).

set_patterns(Patterns) ->
    gen_server:call(?MODULE, {set_patterns, Patterns}, 5000).

reset_to_defaults() ->
    gen_server:call(?MODULE, reset_to_defaults, 5000).

init([]) ->
    ConfigFile = "/data/exclusion_patterns.json",
    Patterns = load_patterns(ConfigFile),
    {ok, #state{
        patterns = Patterns,
        config_file = ConfigFile
    }}.

handle_call({is_excluded, FilePath}, _From, State) ->
    Result = check_exclusion(FilePath, State#state.patterns),
    {reply, Result, State};

handle_call({add_pattern, Pattern}, _From, State) ->
    NewPatterns = case lists:member(Pattern, State#state.patterns) of
        true -> State#state.patterns;
        false -> [Pattern | State#state.patterns]
    end,
    save_patterns(State#state.config_file, NewPatterns),
    {reply, ok, State#state{patterns = NewPatterns}};

handle_call({remove_pattern, Pattern}, _From, State) ->
    NewPatterns = lists:delete(Pattern, State#state.patterns),
    save_patterns(State#state.config_file, NewPatterns),
    {reply, ok, State#state{patterns = NewPatterns}};

handle_call(get_patterns, _From, State) ->
    {reply, {ok, State#state.patterns}, State};

handle_call({set_patterns, Patterns}, _From, State) ->
    save_patterns(State#state.config_file, Patterns),
    {reply, ok, State#state{patterns = Patterns}};

handle_call(reset_to_defaults, _From, State) ->
    save_patterns(State#state.config_file, ?DEFAULT_PATTERNS),
    {reply, ok, State#state{patterns = ?DEFAULT_PATTERNS}}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

check_exclusion(FilePath, Patterns) ->
    lists:any(fun(Pattern) ->
        match_pattern(FilePath, Pattern)
    end, Patterns).

match_pattern(FilePath, Pattern) ->
    PatternParts = string:split(Pattern, "*", all),
    case PatternParts of
        [Pattern] ->
            string:find(FilePath, Pattern) =/= nomatch;
        [Prefix, Suffix] ->
            HasPrefix = case Prefix of
                "" -> true;
                _ -> string:prefix(FilePath, Prefix) =/= nomatch
            end,
            HasSuffix = case Suffix of
                "" -> true;
                _ -> string:find(FilePath, Suffix) =/= nomatch
            end,
            HasPrefix andalso HasSuffix;
        _ ->
            lists:all(fun(Part) ->
                Part =:= "" orelse string:find(FilePath, Part) =/= nomatch
            end, PatternParts)
    end.

load_patterns(ConfigFile) ->
    case file:read_file(ConfigFile) of
        {ok, Content} ->
            try
                jsx:decode(Content, [return_maps])
            catch
                _:_ -> ?DEFAULT_PATTERNS
            end;
        {error, _} ->
            ?DEFAULT_PATTERNS
    end.

save_patterns(ConfigFile, Patterns) ->
    BinaryPatterns = [ensure_binary(P) || P <- Patterns],
    file:write_file(ConfigFile, jsx:encode(BinaryPatterns)).

ensure_binary(S) when is_binary(S) -> S;
ensure_binary(S) when is_list(S) -> list_to_binary(S).
