%%%-------------------------------------------------------------------
%%% @doc ui_customizer Helper Module - Part 4
%%% @end
%%%-------------------------------------------------------------------
-module(ui_customizer_part4).

-export([clean_code_response/1, do_optimize_layout/1]).

clean_code_response(Response) ->
    %% Remove markdown code blocks if present
    Code1 = re:replace(Response, "^```erlang\n", "", [{return, binary}]),
    Code2 = re:replace(Code1, "\n```$", "", [{return, binary}]),
    Code2.

%%====================================================================
%% Layout Optimization
%%====================================================================

do_optimize_layout(HandlerName) ->
    HandlerPath = ?UI_PATH ++ "/" ++ binary_to_list(HandlerName) ++ ".erl",
    
    case file:read_file(HandlerPath) of
        {ok, Content} ->
            Prompt = "Optimize this UI handler for maximum information density.\n\n"
                     "Current code:\n```erlang\n" ++ binary_to_list(Content) ++ "\n```\n\n"
                     "Apply Value Line style principles:\n"
                     "1. Compact layouts with minimal whitespace\n"
                     "2. Dense data tables where appropriate\n"
                     "3. Abbreviated labels only where meaning is clear\n"
                     "4. Multiple data points per row\n"
                     "5. Efficient use of screen real estate\n\n"
                     "Respond with JSON containing old_code and new_code.",
            
            SystemPrompt = "You are a UI density optimization expert. "
                          "Transform UIs to show maximum information in minimum space "
                          "while maintaining readability and usability.",
            
            case lm_client:generate(Prompt, SystemPrompt) of
                {ok, Response} ->
                    apply_ui_improvement(HandlerPath, Content, Response);
                {error, Reason} ->
                    {error, {lm_failed, Reason}}
            end;
        {error, Reason} ->
            {error, {read_failed, Reason}}
    end.

