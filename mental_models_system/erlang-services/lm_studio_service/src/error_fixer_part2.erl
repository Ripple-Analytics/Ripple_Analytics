%%%-------------------------------------------------------------------
%%% @doc error_fixer Helper Module - Part 2
%%% @end
%%%-------------------------------------------------------------------
-module(error_fixer_part2).

-export([parse_erlang_error/1, build_compile_fix_prompt/4, get_compile_fix_system_prompt/0, apply_compile_fix/3, apply_fix_to_file/5, do_fix_build_error/2]).

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

