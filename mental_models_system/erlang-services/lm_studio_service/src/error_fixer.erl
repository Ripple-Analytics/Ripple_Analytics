%%%-------------------------------------------------------------------
%%% @doc Error Fixer - Automatically fixes compilation and runtime errors
%%% 
%%% Monitors for errors and uses LLM to generate fixes:
%%% - Erlang compilation errors
%%% - Docker build failures
%%% - Runtime crashes
%%% - Health check failures
%%% @end
%%%-------------------------------------------------------------------
-module(error_fixer).
-behaviour(gen_server).

-export([start_link/0, fix_compile_error/2, fix_build_error/2, 
         fix_runtime_error/2, get_fix_history/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-record(state, {
    fix_history = [] :: list(),
    total_fixes = 0 :: integer(),
    successful_fixes = 0 :: integer(),
    failed_fixes = 0 :: integer()
}).

%%====================================================================
%% API
%%====================================================================

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% Fix an Erlang compilation error
fix_compile_error(FilePath, ErrorOutput) ->
    gen_server:call(?MODULE, {fix_compile, FilePath, ErrorOutput}, 120000).

%% Fix a Docker build error
fix_build_error(ServiceName, BuildOutput) ->
    gen_server:call(?MODULE, {fix_build, ServiceName, BuildOutput}, 120000).

%% Fix a runtime error/crash
fix_runtime_error(ServiceName, ErrorLog) ->
    gen_server:call(?MODULE, {fix_runtime, ServiceName, ErrorLog}, 120000).

get_fix_history() ->
    gen_server:call(?MODULE, get_history).

%%====================================================================
%% gen_server callbacks
%%====================================================================

init([]) ->
    io:format("[ERROR-FIXER] Error fixer initialized~n"),
    {ok, #state{}}.

handle_call({fix_compile, FilePath, ErrorOutput}, _From, State) ->
    io:format("[ERROR-FIXER] Attempting to fix compile error in ~s~n", [FilePath]),
    
    Result = do_fix_compile_error(FilePath, ErrorOutput),
    
    NewState = record_fix_attempt(State, compile, FilePath, Result),
    {reply, Result, NewState};

handle_call({fix_build, ServiceName, BuildOutput}, _From, State) ->
    io:format("[ERROR-FIXER] Attempting to fix build error for ~s~n", [ServiceName]),
    
    Result = do_fix_build_error(ServiceName, BuildOutput),
    
    NewState = record_fix_attempt(State, build, ServiceName, Result),
    {reply, Result, NewState};

handle_call({fix_runtime, ServiceName, ErrorLog}, _From, State) ->
    io:format("[ERROR-FIXER] Attempting to fix runtime error for ~s~n", [ServiceName]),
    
    Result = do_fix_runtime_error(ServiceName, ErrorLog),
    
    NewState = record_fix_attempt(State, runtime, ServiceName, Result),
    {reply, Result, NewState};

handle_call(get_history, _From, State) ->
    History = #{
        <<"total_fixes">> => State#state.total_fixes,
        <<"successful_fixes">> => State#state.successful_fixes,
        <<"failed_fixes">> => State#state.failed_fixes,
        <<"recent_fixes">> => lists:sublist(State#state.fix_history, 20)
    },
    {reply, {ok, History}, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

%%====================================================================
%% Compile Error Fixing
%%====================================================================

do_fix_compile_error(FilePath, ErrorOutput) ->
    %% Parse the error to understand what's wrong
    ParsedError = parse_erlang_error(ErrorOutput),
    
    %% Read the file content
    case file:read_file(FilePath) of
        {ok, Content} ->
            %% Generate fix using LLM
            Prompt = build_compile_fix_prompt(FilePath, Content, ParsedError, ErrorOutput),
            SystemPrompt = get_compile_fix_system_prompt(),
            
            case lm_client:generate(Prompt, SystemPrompt) of
                {ok, Response} ->
                    apply_compile_fix(FilePath, Content, Response);
                {error, Reason} ->
                    {error, {lm_failed, Reason}}
            end;
        {error, Reason} ->
            {error, {read_failed, Reason}}
    end.

parse_erlang_error(ErrorOutput) ->
    %% Extract line number, error type, and message
    Lines = string:split(ErrorOutput, "\n", all),
    
    %% Look for pattern: filename:line:column: error_type: message
    ErrorInfo = lists:filtermap(fun(Line) ->
        case re:run(Line, "([^:]+):([0-9]+):([0-9]+): (.+)", [{capture, all_but_first, list}]) of
            {match, [_File, LineNum, _Col, Message]} ->
                {true, #{
                    line => list_to_integer(LineNum),
                    message => Message
                }};
            nomatch ->
                %% Try simpler pattern
                case re:run(Line, "([^:]+):([0-9]+): (.+)", [{capture, all_but_first, list}]) of
                    {match, [_File, LineNum, Message]} ->
                        {true, #{
                            line => list_to_integer(LineNum),
                            message => Message
                        }};
                    nomatch ->
                        false
                end
        end
    end, Lines),
    
    #{
        <<"errors">> => ErrorInfo,
        <<"raw">> => ErrorOutput
    }.

build_compile_fix_prompt(FilePath, Content, ParsedError, RawError) ->
    Errors = maps:get(<<"errors">>, ParsedError, []),
    ErrorLines = [io_lib:format("Line ~p: ~s", [maps:get(line, E), maps:get(message, E)]) 
                  || E <- Errors],
    
    "Fix the following Erlang compilation error.\n\n"
    "File: " ++ binary_to_list(FilePath) ++ "\n\n"
    "Errors:\n" ++ lists:flatten(lists:join("\n", ErrorLines)) ++ "\n\n"
    "Raw error output:\n" ++ RawError ++ "\n\n"
    "Current code:\n```erlang\n" ++ binary_to_list(Content) ++ "\n```\n\n"
    "Respond with JSON containing:\n"
    "- fixed: true/false (whether you can fix it)\n"
    "- old_code: the exact problematic code\n"
    "- new_code: the fixed code\n"
    "- explanation: what was wrong and how you fixed it".

get_compile_fix_system_prompt() ->
    "You are an expert Erlang compiler error fixer. "
    "Analyze the error message carefully and provide a precise fix. "
    "Common Erlang errors include:\n"
    "- Syntax errors (missing periods, commas, parentheses)\n"
    "- Unescaped quotes in strings (use \\\" instead of \")\n"
    "- Unsafe variables in case/if expressions\n"
    "- Missing function clauses\n"
    "- Type mismatches\n\n"
    "Your fix must:\n"
    "1. Match the old_code EXACTLY as it appears in the file\n"
    "2. Only change what's necessary to fix the error\n"
    "3. Maintain the original code style\n"
    "Always respond with valid JSON.".

apply_compile_fix(FilePath, Content, Response) ->
    try
        Decoded = jsx:decode(Response, [return_maps]),
        case maps:get(<<"fixed">>, Decoded, false) of
            true ->
                OldCode = maps:get(<<"old_code">>, Decoded, <<>>),
                NewCode = maps:get(<<"new_code">>, Decoded, <<>>),
                
                case binary:match(Content, OldCode) of
                    nomatch ->
                        %% Try with trimmed whitespace
                        TrimmedOld = string:trim(binary_to_list(OldCode)),
                        case string:find(binary_to_list(Content), TrimmedOld) of
                            nomatch ->
                                {error, old_code_not_found};
                            _ ->
                                %% Apply fix with trimmed match
                                apply_fix_to_file(FilePath, Content, 
                                                  list_to_binary(TrimmedOld), NewCode, Decoded)
                        end;
                    _ ->
                        apply_fix_to_file(FilePath, Content, OldCode, NewCode, Decoded)
                end;
            false ->
                Explanation = maps:get(<<"explanation">>, Decoded, <<"Unknown reason">>),
                {error, {cannot_fix, Explanation}}
        end
    catch
        _:_ ->
            {error, {parse_failed, Response}}
    end.

apply_fix_to_file(FilePath, Content, OldCode, NewCode, Decoded) ->
    UpdatedContent = binary:replace(Content, OldCode, NewCode),
    
    %% Verify the fix compiles
    TempFile = binary_to_list(FilePath) ++ ".fixed",
    case file:write_file(TempFile, UpdatedContent) of
        ok ->
            CompileResult = os:cmd("erlc -W " ++ TempFile ++ " 2>&1"),
            file:delete(TempFile),
            
            case string:find(CompileResult, "error") of
                nomatch ->
                    %% Fix compiles! Apply it
                    case file:write_file(FilePath, UpdatedContent) of
                        ok ->
                            io:format("[ERROR-FIXER] Successfully fixed ~s~n", [FilePath]),
                            {ok, #{
                                <<"fixed">> => true,
                                <<"file">> => FilePath,
                                <<"explanation">> => maps:get(<<"explanation">>, Decoded, <<>>)
                            }};
                        {error, Reason} ->
                            {error, {write_failed, Reason}}
                    end;
                _ ->
                    %% Fix didn't work
                    {error, {fix_still_has_errors, CompileResult}}
            end;
        {error, Reason} ->
            {error, {temp_write_failed, Reason}}
    end.

%%====================================================================
%% Build Error Fixing
%%====================================================================

do_fix_build_error(ServiceName, BuildOutput) ->
    %% Parse Docker build error
    ParsedError = parse_docker_error(BuildOutput),
    
    %% Determine the likely cause
    case identify_build_error_type(ParsedError) of
        {compile_error, FilePath, ErrorMsg} ->
            %% It's an Erlang compile error during build
            do_fix_compile_error(FilePath, ErrorMsg);
        {dockerfile_error, Issue} ->
            fix_dockerfile_error(ServiceName, Issue, BuildOutput);
        {dependency_error, Dep} ->
            fix_dependency_error(ServiceName, Dep, BuildOutput);
        unknown ->
            %% Ask LLM to analyze
            ask_lm_for_build_fix(ServiceName, BuildOutput)
    end.

parse_docker_error(BuildOutput) ->
    #{
        <<"has_compile_error">> => string:find(BuildOutput, "error:") =/= nomatch,
        <<"has_rebar_error">> => string:find(BuildOutput, "rebar3") =/= nomatch,
        <<"has_dependency_error">> => string:find(BuildOutput, "dependency") =/= nomatch,
        <<"raw">> => BuildOutput
    }.

identify_build_error_type(ParsedError) ->
    Raw = maps:get(<<"raw">>, ParsedError, ""),
    
    %% Check for Erlang compile error
    case re:run(Raw, "([^\\s]+\\.erl):([0-9]+):.+error", [{capture, all_but_first, list}]) of
        {match, [File, _Line]} ->
            {compile_error, File, Raw};
        nomatch ->
            case maps:get(<<"has_dependency_error">>, ParsedError, false) of
                true -> {dependency_error, Raw};
                false -> unknown
            end
    end.

fix_dockerfile_error(ServiceName, Issue, BuildOutput) ->
    %% Find the Dockerfile
    BasePath = "/repo/mental_models_system/erlang-services",
    DockerfilePath = BasePath ++ "/" ++ ServiceName ++ "/Dockerfile",
    
    case file:read_file(DockerfilePath) of
        {ok, Content} ->
            Prompt = "Fix this Dockerfile error.\n\n"
                     "Service: " ++ ServiceName ++ "\n"
                     "Issue: " ++ Issue ++ "\n"
                     "Build output:\n" ++ BuildOutput ++ "\n\n"
                     "Dockerfile:\n" ++ binary_to_list(Content),
            
            case lm_client:generate(Prompt, "You are a Docker expert. Fix the Dockerfile issue.") of
                {ok, Response} ->
                    %% Parse and apply fix
                    {ok, #{<<"suggestion">> => Response}};
                {error, Reason} ->
                    {error, Reason}
            end;
        {error, _} ->
            {error, dockerfile_not_found}
    end.

fix_dependency_error(ServiceName, _Dep, BuildOutput) ->
    Prompt = "Fix this Erlang dependency error.\n\n"
             "Service: " ++ ServiceName ++ "\n"
             "Build output:\n" ++ BuildOutput,
    
    case lm_client:generate(Prompt, "You are an Erlang/rebar3 expert. Fix the dependency issue.") of
        {ok, Response} ->
            {ok, #{<<"suggestion">> => Response}};
        {error, Reason} ->
            {error, Reason}
    end.

ask_lm_for_build_fix(ServiceName, BuildOutput) ->
    Prompt = "Analyze this Docker build failure and suggest a fix.\n\n"
             "Service: " ++ ServiceName ++ "\n"
             "Build output:\n" ++ BuildOutput,
    
    SystemPrompt = "You are an expert in Docker, Erlang/OTP, and build systems. "
                   "Analyze the build failure and provide a specific fix.",
    
    case lm_client:generate(Prompt, SystemPrompt) of
        {ok, Response} ->
            {ok, #{<<"analysis">> => Response}};
        {error, Reason} ->
            {error, Reason}
    end.

%%====================================================================
%% Runtime Error Fixing
%%====================================================================

do_fix_runtime_error(ServiceName, ErrorLog) ->
    %% Parse the runtime error
    ParsedError = parse_runtime_error(ErrorLog),
    
    Prompt = "Analyze this Erlang runtime error and suggest a fix.\n\n"
             "Service: " ++ ServiceName ++ "\n"
             "Error log:\n" ++ ErrorLog ++ "\n\n"
             "Parsed info: " ++ io_lib:format("~p", [ParsedError]),
    
    SystemPrompt = "You are an expert Erlang/OTP debugger. "
                   "Analyze the runtime error and provide a specific code fix. "
                   "Common runtime errors include:\n"
                   "- badmatch - pattern match failure\n"
                   "- badarg - wrong argument type\n"
                   "- function_clause - no matching function clause\n"
                   "- case_clause - no matching case clause\n"
                   "- timeout - gen_server call timeout\n"
                   "Respond with JSON containing the fix.",
    
    case lm_client:generate(Prompt, SystemPrompt) of
        {ok, Response} ->
            try
                Decoded = jsx:decode(Response, [return_maps]),
                {ok, Decoded}
            catch
                _:_ ->
                    {ok, #{<<"analysis">> => Response}}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

parse_runtime_error(ErrorLog) ->
    #{
        <<"has_badmatch">> => string:find(ErrorLog, "badmatch") =/= nomatch,
        <<"has_badarg">> => string:find(ErrorLog, "badarg") =/= nomatch,
        <<"has_function_clause">> => string:find(ErrorLog, "function_clause") =/= nomatch,
        <<"has_case_clause">> => string:find(ErrorLog, "case_clause") =/= nomatch,
        <<"has_timeout">> => string:find(ErrorLog, "timeout") =/= nomatch
    }.

%%====================================================================
%% Internal
%%====================================================================

record_fix_attempt(State, Type, Target, Result) ->
    Success = case Result of
        {ok, _} -> true;
        {error, _} -> false
    end,
    
    Entry = #{
        <<"type">> => Type,
        <<"target">> => Target,
        <<"success">> => Success,
        <<"timestamp">> => erlang:system_time(second)
    },
    
    State#state{
        fix_history = [Entry | State#state.fix_history],
        total_fixes = State#state.total_fixes + 1,
        successful_fixes = State#state.successful_fixes + (if Success -> 1; true -> 0 end),
        failed_fixes = State#state.failed_fixes + (if Success -> 0; true -> 1 end)
    }.
