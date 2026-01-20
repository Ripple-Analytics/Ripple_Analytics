%%%-------------------------------------------------------------------
%%% @doc ui_customizer Helper Module - Part 3
%%% @end
%%%-------------------------------------------------------------------
-module(ui_customizer_part3).

-export([apply_ui_improvement/3, verify_erlang_syntax/1, do_add_feature/2, build_feature_prompt/3, do_generate_page/2, build_page_generation_prompt/2, get_page_generation_system_prompt/0, create_new_handler/3]).

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

