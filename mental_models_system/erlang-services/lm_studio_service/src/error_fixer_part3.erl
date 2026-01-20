%%%-------------------------------------------------------------------
%%% @doc error_fixer Helper Module - Part 3
%%% @end
%%%-------------------------------------------------------------------
-module(error_fixer_part3).

-export([parse_docker_error/1, identify_build_error_type/1, fix_dockerfile_error/3, fix_dependency_error/3, ask_lm_for_build_fix/2, do_fix_runtime_error/2, parse_runtime_error/1, record_fix_attempt/4]).

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

