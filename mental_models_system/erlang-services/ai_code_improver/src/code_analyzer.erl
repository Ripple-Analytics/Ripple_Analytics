-module(code_analyzer).
-behaviour(gen_server).

-export([start_link/0, analyze_file/1, analyze_directory/1, get_codebase_summary/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-record(state, {
    services_path :: string(),
    analyzed_files :: map(),
    last_scan :: integer()
}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

analyze_file(FilePath) ->
    gen_server:call(?MODULE, {analyze_file, FilePath}, 60000).

analyze_directory(DirPath) ->
    gen_server:call(?MODULE, {analyze_directory, DirPath}, 120000).

get_codebase_summary() ->
    gen_server:call(?MODULE, get_codebase_summary, 30000).

init([]) ->
    ServicesPath = "/app/services",
    {ok, #state{
        services_path = ServicesPath,
        analyzed_files = #{},
        last_scan = 0
    }}.

handle_call({analyze_file, FilePath}, _From, State) ->
    case file:read_file(FilePath) of
        {ok, Content} ->
            Analysis = analyze_erlang_content(FilePath, Content),
            NewAnalyzed = maps:put(FilePath, Analysis, State#state.analyzed_files),
            {reply, {ok, Analysis}, State#state{analyzed_files = NewAnalyzed}};
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;

handle_call({analyze_directory, DirPath}, _From, State) ->
    ErlFiles = find_erlang_files(DirPath),
    Results = lists:map(fun(File) ->
        case file:read_file(File) of
            {ok, Content} ->
                {File, analyze_erlang_content(File, Content)};
            {error, Reason} ->
                {File, {error, Reason}}
        end
    end, ErlFiles),
    NewAnalyzed = lists:foldl(fun({File, Analysis}, Acc) ->
        maps:put(File, Analysis, Acc)
    end, State#state.analyzed_files, Results),
    {reply, {ok, Results}, State#state{analyzed_files = NewAnalyzed, last_scan = erlang:system_time(second)}};

handle_call(get_codebase_summary, _From, State) ->
    Summary = #{
        <<"total_files">> => maps:size(State#state.analyzed_files),
        <<"last_scan">> => State#state.last_scan,
        <<"services_path">> => list_to_binary(State#state.services_path),
        <<"files">> => maps:fold(fun(K, V, Acc) ->
            [#{<<"path">> => list_to_binary(K), <<"analysis">> => V} | Acc]
        end, [], State#state.analyzed_files)
    },
    {reply, {ok, Summary}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

find_erlang_files(DirPath) ->
    case file:list_dir(DirPath) of
        {ok, Files} ->
            lists:flatmap(fun(File) ->
                FullPath = filename:join(DirPath, File),
                case filelib:is_dir(FullPath) of
                    true ->
                        case File of
                            "_build" -> [];
                            ".git" -> [];
                            "deps" -> [];
                            _ -> find_erlang_files(FullPath)
                        end;
                    false ->
                        case filename:extension(File) of
                            ".erl" -> [FullPath];
                            _ -> []
                        end
                end
            end, Files);
        {error, _} ->
            []
    end.

analyze_erlang_content(FilePath, Content) ->
    ContentStr = binary_to_list(Content),
    Lines = string:split(ContentStr, "\n", all),
    LineCount = length(Lines),
    
    ModuleName = extract_module_name(ContentStr),
    Behaviours = extract_behaviours(ContentStr),
    Exports = extract_exports(ContentStr),
    Functions = extract_function_names(ContentStr),
    Records = extract_records(ContentStr),
    
    Complexity = calculate_complexity(ContentStr),
    Issues = detect_issues(ContentStr, FilePath),
    
    #{
        <<"file">> => list_to_binary(FilePath),
        <<"module">> => ModuleName,
        <<"behaviours">> => Behaviours,
        <<"exports">> => Exports,
        <<"functions">> => Functions,
        <<"records">> => Records,
        <<"line_count">> => LineCount,
        <<"complexity">> => Complexity,
        <<"issues">> => Issues
    }.

extract_module_name(Content) ->
    case re:run(Content, "-module\\(([^)]+)\\)", [{capture, [1], list}]) of
        {match, [Name]} -> list_to_binary(Name);
        nomatch -> <<"unknown">>
    end.

extract_behaviours(Content) ->
    case re:run(Content, "-behaviour\\(([^)]+)\\)", [global, {capture, [1], list}]) of
        {match, Matches} -> [list_to_binary(M) || [M] <- Matches];
        nomatch -> []
    end.

extract_exports(Content) ->
    case re:run(Content, "-export\\(\\[([^\\]]+)\\]\\)", [global, {capture, [1], list}]) of
        {match, Matches} -> 
            lists:flatmap(fun([M]) ->
                Funs = string:split(M, ",", all),
                [list_to_binary(string:trim(F)) || F <- Funs]
            end, Matches);
        nomatch -> []
    end.

extract_function_names(Content) ->
    case re:run(Content, "^([a-z][a-zA-Z0-9_]*)\\(", [global, multiline, {capture, [1], list}]) of
        {match, Matches} -> lists:usort([list_to_binary(M) || [M] <- Matches]);
        nomatch -> []
    end.

extract_records(Content) ->
    case re:run(Content, "-record\\(([^,]+),", [global, {capture, [1], list}]) of
        {match, Matches} -> [list_to_binary(string:trim(M)) || [M] <- Matches];
        nomatch -> []
    end.

calculate_complexity(Content) ->
    CaseCount = count_pattern(Content, "case\\s"),
    IfCount = count_pattern(Content, "if\\s"),
    ReceiveCount = count_pattern(Content, "receive\\s"),
    TryCount = count_pattern(Content, "try\\s"),
    FunCount = count_pattern(Content, "fun\\s*\\("),
    
    CaseCount + IfCount + ReceiveCount + TryCount + FunCount.

count_pattern(Content, Pattern) ->
    case re:run(Content, Pattern, [global]) of
        {match, Matches} -> length(Matches);
        nomatch -> 0
    end.

detect_issues(Content, _FilePath) ->
    Issues = [],
    
    Issues1 = case re:run(Content, "-compile\\(export_all\\)") of
        {match, _} -> [#{<<"type">> => <<"warning">>, <<"message">> => <<"Uses export_all">>} | Issues];
        nomatch -> Issues
    end,
    
    Issues2 = case re:run(Content, "catch\\s+_:_") of
        {match, _} -> [#{<<"type">> => <<"warning">>, <<"message">> => <<"Catches all exceptions">>} | Issues1];
        nomatch -> Issues1
    end,
    
    Issues3 = case re:run(Content, "TODO|FIXME|HACK|XXX", [caseless]) of
        {match, _} -> [#{<<"type">> => <<"info">>, <<"message">> => <<"Contains TODO/FIXME markers">>} | Issues2];
        nomatch -> Issues2
    end,
    
    Issues3.
