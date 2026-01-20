%%%-------------------------------------------------------------------
%%% @doc UI Customizer - Autonomous UI improvement and customization
%%% 
%%% Enables the system to modify its own UI:
%%% - Analyze UI handler files
%%% - Generate improved layouts and components
%%% - Add new features and pages
%%% - Optimize information density
%%% - Track changes and rollback if needed
%%% @end
%%%-------------------------------------------------------------------
-module(ui_customizer).
-behaviour(gen_server).

-export([start_link/0, get_status/0, analyze_ui/0, improve_handler/1,
         add_feature/2, generate_page/2, optimize_layout/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-record(state, {
    ui_analysis = #{} :: map(),
    pending_changes = [] :: list(),
    applied_changes = [] :: list(),
    rollback_stack = [] :: list(),
    total_improvements = 0 :: integer()
}).

-define(UI_PATH, "/repo/mental_models_system/erlang-services/desktop_ui/src").

%%====================================================================
%% API
%%====================================================================

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

get_status() ->
    gen_server:call(?MODULE, get_status).

analyze_ui() ->
    gen_server:call(?MODULE, analyze_ui, 60000).

improve_handler(HandlerName) ->
    gen_server:call(?MODULE, {improve_handler, HandlerName}, 120000).

add_feature(HandlerName, FeatureSpec) ->
    gen_server:call(?MODULE, {add_feature, HandlerName, FeatureSpec}, 120000).

generate_page(PageName, PageSpec) ->
    gen_server:call(?MODULE, {generate_page, PageName, PageSpec}, 120000).

optimize_layout(HandlerName) ->
    gen_server:call(?MODULE, {optimize_layout, HandlerName}, 120000).

%%====================================================================
%% gen_server callbacks
%%====================================================================

init([]) ->
    io:format("[UI-CUSTOMIZER] UI Customizer initialized~n"),
    {ok, #state{}}.

handle_call(get_status, _From, State) ->
    Status = #{
        <<"ui_handlers">> => maps:keys(State#state.ui_analysis),
        <<"pending_changes">> => length(State#state.pending_changes),
        <<"applied_changes">> => length(State#state.applied_changes),
        <<"total_improvements">> => State#state.total_improvements
    },
    {reply, {ok, Status}, State};

handle_call(analyze_ui, _From, State) ->
    io:format("[UI-CUSTOMIZER] Analyzing UI handlers...~n"),
    Analysis = do_analyze_ui(),
    {reply, {ok, Analysis}, State#state{ui_analysis = Analysis}};

handle_call({improve_handler, HandlerName}, _From, State) ->
    io:format("[UI-CUSTOMIZER] Improving handler: ~s~n", [HandlerName]),
    Result = do_improve_handler(HandlerName, State#state.ui_analysis),
    NewState = case Result of
        {ok, Change} ->
            State#state{
                applied_changes = [Change | State#state.applied_changes],
                total_improvements = State#state.total_improvements + 1
            };
        _ -> State
    end,
    {reply, Result, NewState};

handle_call({add_feature, HandlerName, FeatureSpec}, _From, State) ->
    io:format("[UI-CUSTOMIZER] Adding feature to ~s: ~p~n", [HandlerName, FeatureSpec]),
    Result = do_add_feature(HandlerName, FeatureSpec),
    NewState = case Result of
        {ok, Change} ->
            State#state{
                applied_changes = [Change | State#state.applied_changes],
                total_improvements = State#state.total_improvements + 1
            };
        _ -> State
    end,
    {reply, Result, NewState};

handle_call({generate_page, PageName, PageSpec}, _From, State) ->
    io:format("[UI-CUSTOMIZER] Generating new page: ~s~n", [PageName]),
    Result = do_generate_page(PageName, PageSpec),
    NewState = case Result of
        {ok, Change} ->
            State#state{
                applied_changes = [Change | State#state.applied_changes],
                total_improvements = State#state.total_improvements + 1
            };
        _ -> State
    end,
    {reply, Result, NewState};

handle_call({optimize_layout, HandlerName}, _From, State) ->
    io:format("[UI-CUSTOMIZER] Optimizing layout for: ~s~n", [HandlerName]),
    Result = do_optimize_layout(HandlerName),
    NewState = case Result of
        {ok, Change} ->
            State#state{
                applied_changes = [Change | State#state.applied_changes],
                total_improvements = State#state.total_improvements + 1
            };
        _ -> State
    end,
    {reply, Result, NewState};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

%%====================================================================
%% UI Analysis
%%====================================================================

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

apply_ui_improvement(HandlerPath, Content, Response) ->
    try
        Decoded = jsx:decode(Response, [return_maps]),
        OldCode = maps:get(<<"old_code">>, Decoded, <<>>),
        NewCode = maps:get(<<"new_code">>, Decoded, <<>>),
        
        case binary:match(Content, OldCode) of
            nomatch ->
                {error, old_code_not_found};
            _ ->
                %% Create backup
                BackupPath = HandlerPath ++ ".backup",
                file:write_file(BackupPath, Content),
                
                %% Apply change
                UpdatedContent = binary:replace(Content, OldCode, NewCode),
                
                %% Verify it compiles
                case verify_erlang_syntax(UpdatedContent) of
                    ok ->
                        file:write_file(HandlerPath, UpdatedContent),
                        io:format("[UI-CUSTOMIZER] Applied improvement to ~s~n", [HandlerPath]),
                        {ok, #{
                            <<"handler">> => list_to_binary(HandlerPath),
                            <<"description">> => maps:get(<<"description">>, Decoded, <<>>),
                            <<"backup">> => list_to_binary(BackupPath)
                        }};
                    {error, SyntaxError} ->
                        {error, {syntax_error, SyntaxError}}
                end
        end
    catch
        _:_ ->
            {error, {parse_failed, Response}}
    end.

verify_erlang_syntax(Content) ->
    TempFile = "/tmp/ui_verify_" ++ integer_to_list(erlang:unique_integer([positive])) ++ ".erl",
    case file:write_file(TempFile, Content) of
        ok ->
            Result = os:cmd("erlc -W " ++ TempFile ++ " 2>&1"),
            file:delete(TempFile),
            case string:find(Result, "error") of
                nomatch -> ok;
                _ -> {error, Result}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

%%====================================================================
%% Feature Addition
%%====================================================================

do_add_feature(HandlerName, FeatureSpec) ->
    HandlerPath = ?UI_PATH ++ "/" ++ binary_to_list(HandlerName) ++ ".erl",
    
    case file:read_file(HandlerPath) of
        {ok, Content} ->
            Prompt = build_feature_prompt(HandlerName, Content, FeatureSpec),
            SystemPrompt = "You are an expert at adding features to Erlang/Cowboy UI handlers. "
                          "Generate the code to add the requested feature. "
                          "Ensure all HTML quotes are escaped. "
                          "Respond with JSON containing old_code and new_code.",
            
            case lm_client:generate(Prompt, SystemPrompt) of
                {ok, Response} ->
                    apply_ui_improvement(HandlerPath, Content, Response);
                {error, Reason} ->
                    {error, {lm_failed, Reason}}
            end;
        {error, Reason} ->
            {error, {read_failed, Reason}}
    end.

build_feature_prompt(HandlerName, Content, FeatureSpec) ->
    "Add this feature to the UI handler.\n\n"
    "Handler: " ++ binary_to_list(HandlerName) ++ "\n"
    "Feature: " ++ binary_to_list(maps:get(<<"name">>, FeatureSpec, <<>>)) ++ "\n"
    "Description: " ++ binary_to_list(maps:get(<<"description">>, FeatureSpec, <<>>)) ++ "\n\n"
    "Current code:\n```erlang\n" ++ binary_to_list(Content) ++ "\n```\n\n"
    "Add the feature by modifying the existing code.".

%%====================================================================
%% Page Generation
%%====================================================================

do_generate_page(PageName, PageSpec) ->
    HandlerPath = ?UI_PATH ++ "/" ++ binary_to_list(PageName) ++ "_handler.erl",
    
    %% Check if handler already exists
    case filelib:is_file(HandlerPath) of
        true ->
            {error, handler_already_exists};
        false ->
            Prompt = build_page_generation_prompt(PageName, PageSpec),
            SystemPrompt = get_page_generation_system_prompt(),
            
            case lm_client:generate(Prompt, SystemPrompt) of
                {ok, Response} ->
                    create_new_handler(HandlerPath, PageName, Response);
                {error, Reason} ->
                    {error, {lm_failed, Reason}}
            end
    end.

build_page_generation_prompt(PageName, PageSpec) ->
    "Generate a new Erlang/Cowboy UI handler.\n\n"
    "Page name: " ++ binary_to_list(PageName) ++ "\n"
    "Title: " ++ binary_to_list(maps:get(<<"title">>, PageSpec, PageName)) ++ "\n"
    "Description: " ++ binary_to_list(maps:get(<<"description">>, PageSpec, <<>>)) ++ "\n"
    "Features: " ++ io_lib:format("~p", [maps:get(<<"features">>, PageSpec, [])]) ++ "\n\n"
    "Generate a complete Erlang module that:\n"
    "1. Implements cowboy_handler behaviour\n"
    "2. Returns HTML with the requested features\n"
    "3. Uses the same style as other handlers (cards, buttons, etc.)\n"
    "4. Includes necessary JavaScript for interactivity\n"
    "5. Properly escapes all HTML quotes\n\n"
    "Return ONLY the Erlang code, no JSON wrapper.".

get_page_generation_system_prompt() ->
    "You are an expert Erlang/Cowboy developer. "
    "Generate complete, working handler modules. "
    "Follow the pattern of existing handlers in the project. "
    "Use html_templates:wrap_page/2 for consistent styling. "
    "Ensure all HTML attribute quotes are escaped as \\\".".

create_new_handler(HandlerPath, PageName, Response) ->
    %% Clean up the response (remove markdown code blocks if present)
    CleanCode = clean_code_response(Response),
    
    %% Verify syntax
    case verify_erlang_syntax(CleanCode) of
        ok ->
            case file:write_file(HandlerPath, CleanCode) of
                ok ->
                    io:format("[UI-CUSTOMIZER] Created new handler: ~s~n", [HandlerPath]),
                    %% TODO: Also update routing in desktop_ui_app.erl
                    {ok, #{
                        <<"handler">> => list_to_binary(HandlerPath),
                        <<"page">> => PageName,
                        <<"note">> => <<"Remember to add route to desktop_ui_app.erl">>
                    }};
                {error, Reason} ->
                    {error, {write_failed, Reason}}
            end;
        {error, SyntaxError} ->
            {error, {syntax_error, SyntaxError}}
    end.

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
