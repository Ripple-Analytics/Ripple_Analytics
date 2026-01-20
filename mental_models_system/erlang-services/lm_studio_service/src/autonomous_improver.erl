%%%-------------------------------------------------------------------
%%% @doc Autonomous Improver - Code improvement generation
%%% @end
%%%-------------------------------------------------------------------
-module(autonomous_improver).

-export([generate_improvements/2, generate_improvement_for_file/2]).

-define(MAX_IMPROVEMENTS_PER_CYCLE, 3).

%%====================================================================
%% API
%%====================================================================

generate_improvements(AnalysisResult, KnowledgeBase) ->
    Opportunities = maps:get(<<"improvement_opportunities">>, AnalysisResult, []),
    TargetFiles = lists:sublist(Opportunities, ?MAX_IMPROVEMENTS_PER_CYCLE),
    
    lists:filtermap(fun(FilePath) ->
        case generate_improvement_for_file(FilePath, KnowledgeBase) of
            {ok, Improvement} -> {true, Improvement};
            {error, _} -> false
        end
    end, TargetFiles).

generate_improvement_for_file(FilePath, KnowledgeBase) ->
    case file:read_file(FilePath) of
        {ok, Content} ->
            Prompt = build_prompt(FilePath, Content, KnowledgeBase),
            SystemPrompt = get_system_prompt(),
            case lm_client:generate(Prompt, SystemPrompt) of
                {ok, Response} -> parse_response(FilePath, Response);
                {error, Reason} -> {error, Reason}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

%%====================================================================
%% Internal
%%====================================================================

build_prompt(FilePath, Content, KnowledgeBase) ->
    Knowledge = extract_relevant_knowledge(FilePath, KnowledgeBase),
    "Analyze this Erlang file and suggest ONE specific improvement.\n\n"
    "File: " ++ binary_to_list(FilePath) ++ "\n\n"
    "Best practices:\n" ++ format_knowledge(Knowledge) ++ "\n\n"
    "Code:\n```erlang\n" ++ binary_to_list(Content) ++ "\n```\n\n"
    "Respond with JSON: description, old_code, new_code, benefit, risk_level".

get_system_prompt() ->
    "You are an expert Erlang/OTP developer. "
    "Improvements must be: safe, incremental, testable, beneficial. "
    "Always respond with valid JSON. old_code must match EXACTLY.".

extract_relevant_knowledge(FilePath, KnowledgeBase) ->
    FileType = determine_file_type(FilePath),
    Keys = case FileType of
        handler -> [<<"cowboy_handler_patterns">>, <<"error_handling_patterns">>];
        gen_server -> [<<"gen_server_optimization">>, <<"supervision_tree_design">>];
        _ -> [<<"erlang_otp_best_practices">>]
    end,
    maps:with(Keys, KnowledgeBase).

determine_file_type(FilePath) ->
    case binary:match(FilePath, <<"_handler.erl">>) of
        nomatch ->
            case binary:match(FilePath, <<"_sup.erl">>) of
                nomatch -> other;
                _ -> supervisor
            end;
        _ -> handler
    end.

format_knowledge(KnowledgeMap) when map_size(KnowledgeMap) =:= 0 ->
    "No specific knowledge available yet.";
format_knowledge(KnowledgeMap) ->
    lists:flatten([io_lib:format("~s: ~p~n", [K, V]) || {K, V} <- maps:to_list(KnowledgeMap)]).

parse_response(FilePath, Response) ->
    try
        Decoded = jsx:decode(Response, [return_maps]),
        {ok, #{
            <<"file">> => FilePath,
            <<"description">> => maps:get(<<"description">>, Decoded, <<>>),
            <<"old_code">> => maps:get(<<"old_code">>, Decoded, <<>>),
            <<"new_code">> => maps:get(<<"new_code">>, Decoded, <<>>),
            <<"benefit">> => maps:get(<<"benefit">>, Decoded, <<>>),
            <<"risk_level">> => maps:get(<<"risk_level">>, Decoded, <<"medium">>),
            <<"timestamp">> => erlang:system_time(second)
        }}
    catch
        _:_ -> {error, parse_failed}
    end.
