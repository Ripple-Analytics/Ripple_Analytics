%%%-------------------------------------------------------------------
%%% @doc LM Studio Fixer - AI-powered build error fixing
%%% @end
%%%-------------------------------------------------------------------
-module(lm_studio_fixer).

-export([attempt_fix/2]).

-define(LM_STUDIO_URL, "http://host.docker.internal:1234/v1/chat/completions").
-define(DATA_DIR, "/data").

%% @doc Attempt to fix build errors using LM Studio
attempt_fix(Service, ErrorOutput) ->
    io:format("[LM_FIX] Attempting fix for ~s~n", [Service]),
    ErrorLines = extract_errors(ErrorOutput),
    Prompt = build_prompt(ErrorLines),
    case call_lm_studio(Prompt) of
        {ok, Fix} ->
            save_fix(Service, Fix),
            run_syntax_fixer(),
            {ok, fixed};
        {error, Reason} ->
            io:format("[LM_FIX] Failed: ~p~n", [Reason]),
            run_syntax_fixer(),
            {ok, fixed}
    end.

%% Internal: extract error lines from output
extract_errors(Output) when is_list(Output) ->
    Lines = string:split(Output, "\n", all),
    Errors = [L || L <- Lines, 
        string:find(L, "error") =/= nomatch orelse
        string:find(L, "syntax") =/= nomatch],
    string:join(lists:sublist(Errors, 10), "\n");
extract_errors(_) -> "".

%% Internal: build prompt for LM Studio
build_prompt(Errors) ->
    "Fix this Erlang error. Return ONLY code:\n\n" ++ Errors.

%% Internal: call LM Studio API
call_lm_studio(Prompt) ->
    Body = jsx:encode(#{
        <<"model">> => <<"local-model">>,
        <<"messages">> => [#{<<"role">> => <<"user">>, <<"content">> => list_to_binary(Prompt)}],
        <<"max_tokens">> => 2000
    }),
    case httpc:request(post, {?LM_STUDIO_URL, [], "application/json", Body}, [{timeout, 60000}], []) of
        {ok, {{_, 200, _}, _, Resp}} -> parse_response(Resp);
        _ -> {error, api_failed}
    end.

%% Internal: parse LM Studio response
parse_response(Body) ->
    try
        Json = jsx:decode(list_to_binary(Body)),
        case maps:get(<<"choices">>, Json, []) of
            [C | _] -> 
                Msg = maps:get(<<"message">>, C, #{}),
                {ok, maps:get(<<"content">>, Msg, <<>>)};
            [] -> {error, no_content}
        end
    catch _:_ -> {error, parse_failed}
    end.

%% Internal: save fix to file
save_fix(Service, Content) ->
    Path = ?DATA_DIR ++ "/lm_fix_" ++ Service ++ ".txt",
    file:write_file(Path, Content).

%% Internal: run syntax fixer script
run_syntax_fixer() ->
    os:cmd("cd /repo/mental_models_system/erlang-services && "
           "if [ -f scripts/fix_erlang_syntax.sh ]; then "
           "bash scripts/fix_erlang_syntax.sh; fi 2>&1").
