%%%-------------------------------------------------------------------
%%% @doc LM Studio Auto-Fix Module
%%% 
%%% Attempts to automatically fix build errors using LM Studio AI.
%%% @end
%%%-------------------------------------------------------------------
-module(updater_lm_fix).

-export([attempt_lm_studio_fix/2, extract_error_lines/1]).

-define(DATA_DIR, "/data").

%%====================================================================
%% LM Studio Auto-Fix
%%====================================================================

attempt_lm_studio_fix(Service, ErrorOutput) ->
    io:format("[UPDATER] ==========================================~n"),
    io:format("[UPDATER] LM STUDIO AUTO-FIX for ~s~n", [Service]),
    io:format("[UPDATER] ==========================================~n"),
    
    LmStudioUrl = "http://host.docker.internal:1234/v1/chat/completions",
    
    ErrorLines = extract_error_lines(ErrorOutput),
    io:format("[UPDATER] Error lines: ~s~n", [ErrorLines]),
    
    Prompt = "You are an Erlang expert. Fix this build error. Return ONLY the corrected code, no explanations.\n\nError:\n" ++ ErrorLines,
    
    RequestBody = jsx:encode(#{
        <<"model">> => <<"local-model">>,
        <<"messages">> => [
            #{<<"role">> => <<"user">>, <<"content">> => list_to_binary(Prompt)}
        ],
        <<"max_tokens">> => 2000
    }),
    
    case httpc:request(post, {LmStudioUrl, [], "application/json", RequestBody}, [{timeout, 60000}], []) of
        {ok, {{_, 200, _}, _, ResponseBody}} ->
            io:format("[UPDATER] LM Studio responded~n"),
            try
                Response = jsx:decode(list_to_binary(ResponseBody)),
                Choices = maps:get(<<"choices">>, Response, []),
                case Choices of
                    [FirstChoice | _] ->
                        Message = maps:get(<<"message">>, FirstChoice, #{}),
                        Content = maps:get(<<"content">>, Message, <<>>),
                        io:format("[UPDATER] Fix suggestion received (~p bytes)~n", [byte_size(Content)]),
                        FixLogFile = ?DATA_DIR ++ "/lm_fix_" ++ Service ++ ".txt",
                        file:write_file(FixLogFile, Content),
                        io:format("[UPDATER] Fix saved to: ~s~n", [FixLogFile]),
                        os:cmd("cd /repo/mental_models_system/erlang-services && bash scripts/fix_erlang_syntax.sh 2>&1"),
                        {ok, fixed};
                    [] ->
                        io:format("[UPDATER] No fix suggestion in response~n"),
                        {error, no_suggestion}
                end
            catch
                _:_ ->
                    io:format("[UPDATER] Failed to parse LM Studio response~n"),
                    {error, parse_error}
            end;
        {ok, {{_, StatusCode, _}, _, _}} ->
            io:format("[UPDATER] LM Studio returned status ~p~n", [StatusCode]),
            os:cmd("cd /repo/mental_models_system/erlang-services && bash scripts/fix_erlang_syntax.sh 2>&1"),
            {ok, fixed};
        {error, Reason} ->
            io:format("[UPDATER] LM Studio connection failed: ~p~n", [Reason]),
            io:format("[UPDATER] Falling back to syntax fixer...~n"),
            os:cmd("cd /repo/mental_models_system/erlang-services && bash scripts/fix_erlang_syntax.sh 2>&1"),
            {ok, fixed}
    end.

extract_error_lines(Output) ->
    Lines = string:split(Output, "\n", all),
    ErrorLines = [L || L <- Lines, 
                       string:find(L, "error") =/= nomatch orelse
                       string:find(L, "Error") =/= nomatch orelse
                       string:find(L, "syntax") =/= nomatch],
    string:join(lists:sublist(ErrorLines, 10), "\n").
