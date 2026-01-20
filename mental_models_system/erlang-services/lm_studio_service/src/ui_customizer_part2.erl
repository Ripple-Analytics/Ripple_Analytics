%%%-------------------------------------------------------------------
%%% @doc ui_customizer Helper Module - Part 2
%%% @end
%%%-------------------------------------------------------------------
-module(ui_customizer_part2).

-export([do_analyze_ui/0, analyze_handler/1, has_html_content/1, has_javascript/1, has_css/1, extract_components/1, count_buttons/1, count_forms/1, extract_api_calls/1, estimate_ui_complexity/1, do_improve_handler/2, build_ui_improvement_prompt/3, get_ui_improvement_system_prompt/0]).

do_analyze_ui() ->
    %% Find all UI handlers
    Handlers = filelib:wildcard(?UI_PATH ++ "/*_handler.erl"),
    
    %% Analyze each handler
    Analysis = lists:foldl(fun(HandlerPath, Acc) ->
        HandlerName = filename:basename(HandlerPath, ".erl"),
        case analyze_handler(HandlerPath) of
            {ok, Info} ->
                maps:put(list_to_binary(HandlerName), Info, Acc);
            {error, _} ->
                Acc
        end
    end, #{}, Handlers),
    
    Analysis.

analyze_handler(HandlerPath) ->
    case file:read_file(HandlerPath) of
        {ok, Content} ->
            %% Extract information about the handler
            Info = #{
                <<"path">> => list_to_binary(HandlerPath),
                <<"size">> => byte_size(Content),
                <<"has_html">> => has_html_content(Content),
                <<"has_javascript">> => has_javascript(Content),
                <<"has_css">> => has_css(Content),
                <<"components">> => extract_components(Content),
                <<"buttons">> => count_buttons(Content),
                <<"forms">> => count_forms(Content),
                <<"api_calls">> => extract_api_calls(Content),
                <<"complexity">> => estimate_ui_complexity(Content)
            },
            {ok, Info};
        {error, Reason} ->
            {error, Reason}
    end.

has_html_content(Content) ->
    binary:match(Content, <<"<div">>) =/= nomatch orelse
    binary:match(Content, <<"<span">>) =/= nomatch.

has_javascript(Content) ->
    binary:match(Content, <<"<script">>) =/= nomatch orelse
    binary:match(Content, <<"function">>) =/= nomatch.

has_css(Content) ->
    binary:match(Content, <<"style=">>) =/= nomatch orelse
    binary:match(Content, <<"class=">>) =/= nomatch.

extract_components(Content) ->
    %% Look for common UI components
    Components = [
        {<<"cards">>, length(binary:matches(Content, <<"class=\\\"card\\\"">>))},
        {<<"buttons">>, length(binary:matches(Content, <<"class=\\\"btn">>))},
        {<<"inputs">>, length(binary:matches(Content, <<"<input">>))},
        {<<"tables">>, length(binary:matches(Content, <<"<table">>))},
        {<<"charts">>, length(binary:matches(Content, <<"Chart">>))}
    ],
    maps:from_list([{K, V} || {K, V} <- Components, V > 0]).

count_buttons(Content) ->
    length(binary:matches(Content, <<"<button">>)) +
    length(binary:matches(Content, <<"class=\\\"btn">>)).

count_forms(Content) ->
    length(binary:matches(Content, <<"<form">>)) +
    length(binary:matches(Content, <<"<textarea">>)) +
    length(binary:matches(Content, <<"<select">>)).

extract_api_calls(Content) ->
    %% Find fetch() calls
    FetchCalls = length(binary:matches(Content, <<"fetch(">>)),
    %% Find API paths
    ApiPaths = length(binary:matches(Content, <<"/api/">>)),
    #{<<"fetch_calls">> => FetchCalls, <<"api_paths">> => ApiPaths}.

estimate_ui_complexity(Content) ->
    %% Simple complexity score
    Lines = length(binary:split(Content, <<"\n">>, [global])),
    JsBlocks = length(binary:matches(Content, <<"<script">>)),
    Conditionals = length(binary:matches(Content, <<"if (">>)) +
                   length(binary:matches(Content, <<"? ">>)),
    
    Score = Lines div 50 + JsBlocks * 5 + Conditionals,
    if
        Score > 50 -> <<"high">>;
        Score > 20 -> <<"medium">>;
        true -> <<"low">>
    end.

%%====================================================================
%% UI Improvement
%%====================================================================

do_improve_handler(HandlerName, UiAnalysis) ->
    HandlerPath = ?UI_PATH ++ "/" ++ binary_to_list(HandlerName) ++ ".erl",
    
    case file:read_file(HandlerPath) of
        {ok, Content} ->
            %% Get analysis for this handler
            Analysis = maps:get(HandlerName, UiAnalysis, #{}),
            
            %% Build improvement prompt
            Prompt = build_ui_improvement_prompt(HandlerName, Content, Analysis),
            SystemPrompt = get_ui_improvement_system_prompt(),
            
            case lm_client:generate(Prompt, SystemPrompt) of
                {ok, Response} ->
                    apply_ui_improvement(HandlerPath, Content, Response);
                {error, Reason} ->
                    {error, {lm_failed, Reason}}
            end;
        {error, Reason} ->
            {error, {read_failed, Reason}}
    end.

build_ui_improvement_prompt(HandlerName, Content, Analysis) ->
    "Improve this Erlang/Cowboy UI handler.\n\n"
    "Handler: " ++ binary_to_list(HandlerName) ++ "\n"
    "Analysis: " ++ io_lib:format("~p", [Analysis]) ++ "\n\n"
    "Current code:\n```erlang\n" ++ binary_to_list(Content) ++ "\n```\n\n"
    "Suggest ONE specific improvement that:\n"
    "1. Increases information density (Value Line style)\n"
    "2. Improves user experience\n"
    "3. Adds useful functionality\n"
    "4. Maintains the existing code style\n\n"
    "Respond with JSON:\n"
    "- description: what the improvement does\n"
    "- old_code: exact code to replace\n"
    "- new_code: improved code\n"
    "- benefit: why this is better".

get_ui_improvement_system_prompt() ->
    "You are an expert UI/UX developer specializing in information-dense interfaces. "
    "You understand Erlang/Cowboy handlers that generate HTML. "
    "Your improvements should:\n"
    "1. Follow Value Line style - maximum information in minimum space\n"
    "2. Use full descriptive button text (no abbreviations)\n"
    "3. Maintain professional appearance\n"
    "4. Ensure all quotes in HTML are properly escaped (\\\")\n"
    "5. Keep JavaScript inline and functional\n"
    "Always respond with valid JSON.".

