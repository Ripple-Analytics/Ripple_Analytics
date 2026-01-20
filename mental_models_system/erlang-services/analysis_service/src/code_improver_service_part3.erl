%%%-------------------------------------------------------------------
%%% @doc code_improver_service Helper Module - Part 3
%%% @end
%%%-------------------------------------------------------------------
-module(code_improver_service_part3).

-export([generate_improvement/2, call_lm_studio/3, extract_erlang_code/1, apply_and_validate/2, validate_erlang_syntax/1, compile_module/1, add_to_history/3, format_status/1, format_status/2, notify_improvement/1, format_datetime/1]).

generate_improvement(ModulePath, State) ->
    io:format("[CodeImprover] Generating improvement for ~s~n", [ModulePath]),
    
    BaseDir = code:priv_dir(analysis_service),
    SrcDir = filename:join([filename:dirname(BaseDir), "src"]),
    FullPath = filename:join(SrcDir, binary_to_list(ModulePath)),
    
    case file:read_file(FullPath) of
        {ok, CurrentCode} ->
            {SystemPrompt, UserPrompt} = prompt_templates:get_improvement_prompt(ModulePath, CurrentCode),
            
            case call_lm_studio(State#state.lm_studio_url, SystemPrompt, UserPrompt) of
                {ok, ImprovedCode} ->
                    {ok, #{
                        <<"module_path">> => ModulePath,
                        <<"full_path">> => list_to_binary(FullPath),
                        <<"original_code">> => CurrentCode,
                        <<"improved_code">> => ImprovedCode,
                        <<"generated_at">> => format_datetime(calendar:local_time())
                    }};
                {error, Reason} ->
                    {error, Reason}
            end;
        {error, Reason} ->
            {error, {file_read_error, Reason}}
    end.

call_lm_studio(Url, SystemPrompt, UserPrompt) ->
    ApiUrl = Url ++ "/v1/chat/completions",
    Headers = [{<<"content-type">>, <<"application/json">>}],
    
    RequestBody = jsx:encode(#{
        <<"model">> => <<"local-model">>,
        <<"messages">> => [
            #{<<"role">> => <<"system">>, <<"content">> => SystemPrompt},
            #{<<"role">> => <<"user">>, <<"content">> => UserPrompt}
        ],
        <<"max_tokens">> => 4000,
        <<"temperature">> => 0.3
    }),
    
    case hackney:request(post, list_to_binary(ApiUrl), Headers, RequestBody, [{timeout, 120000}]) of
        {ok, 200, _, ClientRef} ->
            {ok, RespBody} = hackney:body(ClientRef),
            Response = jsx:decode(RespBody, [return_maps]),
            Content = maps:get(<<"content">>, 
                        maps:get(<<"message">>, 
                            hd(maps:get(<<"choices">>, Response)))),
            {ok, extract_erlang_code(Content)};
        {ok, Status, _, _} ->
            {error, {http_error, Status}};
        {error, Reason} ->
            {error, Reason}
    end.

extract_erlang_code(Content) ->
    case binary:match(Content, <<"```erlang">>) of
        {Start, Len} ->
            AfterStart = binary:part(Content, Start + Len, byte_size(Content) - Start - Len),
            case binary:match(AfterStart, <<"```">>) of
                {End, _} ->
                    binary:part(AfterStart, 0, End);
                nomatch ->
                    AfterStart
            end;
        nomatch ->
            Content
    end.

apply_and_validate(Improvement, _State) ->
    FullPath = binary_to_list(maps:get(<<"full_path">>, Improvement)),
    ImprovedCode = maps:get(<<"improved_code">>, Improvement),
    OriginalCode = maps:get(<<"original_code">>, Improvement),
    
    case validate_erlang_syntax(ImprovedCode) of
        ok ->
            BackupPath = FullPath ++ ".backup",
            file:write_file(BackupPath, OriginalCode),
            
            case file:write_file(FullPath, ImprovedCode) of
                ok ->
                    case compile_module(FullPath) of
                        ok ->
                            file:delete(BackupPath),
                            {ok, #{
                                <<"status">> => <<"applied">>,
                                <<"path">> => list_to_binary(FullPath)
                            }};
                        {error, CompileError} ->
                            file:write_file(FullPath, OriginalCode),
                            file:delete(BackupPath),
                            {error, {compile_error, CompileError}}
                    end;
                {error, WriteError} ->
                    {error, {write_error, WriteError}}
            end;
        {error, SyntaxError} ->
            {error, {syntax_error, SyntaxError}}
    end.

validate_erlang_syntax(Code) ->
    TempFile = "/tmp/code_improver_temp.erl",
    file:write_file(TempFile, Code),
    case erl_scan:string(binary_to_list(Code)) of
        {ok, Tokens, _} ->
            case erl_parse:parse_form(Tokens) of
                {ok, _} -> ok;
                {error, _} ->
                    case compile:file(TempFile, [return_errors, binary]) of
                        {ok, _, _} -> ok;
                        {error, Errors, _} -> {error, Errors}
                    end
            end;
        {error, ErrorInfo, _} ->
            {error, ErrorInfo}
    end.

compile_module(FilePath) ->
    case compile:file(FilePath, [return_errors]) of
        {ok, _Module} -> ok;
        {ok, _Module, _Warnings} -> ok;
        {error, Errors, _Warnings} -> {error, Errors}
    end.

add_to_history(Improvement, Status, History) ->
    Entry = #{
        <<"module">> => maps:get(<<"module_path">>, Improvement),
        <<"status">> => format_status(Status),
        <<"timestamp">> => format_datetime(calendar:local_time())
    },
    lists:sublist([Entry | History], ?IMPROVEMENT_HISTORY_SIZE).

format_status(success) -> <<"success">>;
format_status({failed, Reason}) -> 
    list_to_binary(io_lib:format("failed: ~p", [Reason])).

notify_improvement(Improvement) ->
    try
        notification_service:notify(code_improvement, #{
            <<"module">> => maps:get(<<"module_path">>, Improvement),
            <<"message">> => <<"Code improvement applied successfully">>
        })
    catch
        _:_ -> ok
    end.

format_datetime(undefined) -> <<"never">>;
