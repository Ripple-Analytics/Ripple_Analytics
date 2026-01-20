%%%-------------------------------------------------------------------
%%% @doc Prompt Templates - Design Philosophy and Code Generation Prompts
%%% 
%%% Contains carefully crafted prompts for autonomous code improvement
%%% that follow the existing codebase design philosophy.
%%% @end
%%%-------------------------------------------------------------------
-module(prompt_templates).

-export([get_system_prompt/0, get_improvement_prompt/2, get_feature_prompt/2]).
-export([get_code_review_prompt/1, get_refactor_prompt/2, get_bug_fix_prompt/2]).
-export([get_design_philosophy/0]).

%%====================================================================
%% Design Philosophy
%%====================================================================

get_design_philosophy() ->
    <<"
RIPPLE ANALYTICS - ERLANG/OTP DESIGN PHILOSOPHY

1. OTP PATTERNS:
   - Use gen_server for stateful services with start_link/0, init/1, handle_call/3, handle_cast/2
   - Use supervisor trees with one_for_one strategy for fault tolerance
   - Use application behaviour for service entry points
   - Processes should be lightweight and focused on single responsibility

2. HTTP HANDLERS (Cowboy):
   - Export init/2 as the entry point
   - Use cowboy_req:method/1 to dispatch by HTTP method
   - Return {ok, Req, State} from handlers
   - Use jsx:encode/1 for JSON responses
   - Always include CORS headers for API endpoints

3. DATA STRUCTURES:
   - Use maps for structured data: #{<<\"key\">> => Value}
   - Use binary strings for text: <<\"text\">>
   - Use lists for collections
   - Use records for internal state in gen_servers

4. ERROR HANDLING:
   - Return {ok, Result} or {error, Reason} tuples
   - Use try/catch for external calls
   - Log errors with io:format for debugging
   - Provide fallback behavior when services unavailable

5. NAMING CONVENTIONS:
   - Module names: lowercase_with_underscores
   - Function names: lowercase_with_underscores
   - Variables: CamelCase
   - Constants: UPPERCASE_WITH_UNDERSCORES (as macros)

6. CODE STYLE:
   - Keep functions short and focused
   - Use pattern matching in function heads
   - Prefer guards over if expressions
   - Document with @doc and @spec

7. MICROSERVICES ARCHITECTURE:
   - Each service runs on its own port
   - Services communicate via HTTP/REST
   - Use blue-green deployment for zero-downtime updates
   - Health endpoints at /health for monitoring
">>.

%%====================================================================
%% System Prompts
%%====================================================================

get_system_prompt() ->
    Philosophy = get_design_philosophy(),
    <<"You are an expert Erlang/OTP developer working on the Ripple Analytics mental models system. You write clean, idiomatic Erlang code following OTP best practices.

", Philosophy/binary, "

When generating code:
- Follow the exact patterns shown in existing code
- Use binary strings for all text values
- Include proper error handling
- Add module documentation
- Keep changes minimal and focused
- Ensure code compiles without errors
- Do not add comments unless specifically requested

You output ONLY valid Erlang code. No explanations, no markdown, just code.">>.

%%====================================================================
%% Improvement Prompts
%%====================================================================

get_improvement_prompt(ModuleName, CurrentCode) ->
    SystemPrompt = get_system_prompt(),
    UserPrompt = <<"Analyze this Erlang module and suggest ONE small improvement that would make it better. The improvement should be:
- Safe and non-breaking
- Following the design philosophy
- Improving code quality, performance, or maintainability

Module: ", ModuleName/binary, "

Current code:
```erlang
", CurrentCode/binary, "
```

Output the COMPLETE improved module code. Only output valid Erlang code, nothing else.">>,
    {SystemPrompt, UserPrompt}.

get_feature_prompt(FeatureDescription, Context) ->
    SystemPrompt = get_system_prompt(),
    UserPrompt = <<"Create a new Erlang module that implements the following feature:

Feature: ", FeatureDescription/binary, "

Context about existing codebase:
", Context/binary, "

Requirements:
- Follow the design philosophy exactly
- Include proper module documentation
- Export all public functions
- Handle errors gracefully
- Use gen_server if state is needed

Output the COMPLETE module code. Only output valid Erlang code, nothing else.">>,
    {SystemPrompt, UserPrompt}.

get_code_review_prompt(Code) ->
    SystemPrompt = get_system_prompt(),
    UserPrompt = <<"Review this Erlang code for issues:

```erlang
", Code/binary, "
```

Output a JSON object with:
{
  \"has_issues\": true/false,
  \"issues\": [
    {\"type\": \"error|warning|suggestion\", \"line\": N, \"message\": \"description\"}
  ],
  \"overall_quality\": 1-10
}

Only output valid JSON, nothing else.">>,
    {SystemPrompt, UserPrompt}.

get_refactor_prompt(ModuleName, CurrentCode) ->
    SystemPrompt = get_system_prompt(),
    UserPrompt = <<"Refactor this Erlang module to improve its structure while maintaining exact same functionality:

Module: ", ModuleName/binary, "

Current code:
```erlang
", CurrentCode/binary, "
```

Refactoring goals:
- Extract repeated code into helper functions
- Improve function naming
- Simplify complex expressions
- Better organize exports

Output the COMPLETE refactored module code. Only output valid Erlang code, nothing else.">>,
    {SystemPrompt, UserPrompt}.

get_bug_fix_prompt(ModuleName, ErrorDescription) ->
    SystemPrompt = get_system_prompt(),
    UserPrompt = <<"Fix the bug in this Erlang module:

Module: ", ModuleName/binary, "

Error description:
", ErrorDescription/binary, "

Analyze the error and provide the corrected code. Output ONLY the specific function(s) that need to be fixed, with proper Erlang syntax.">>,
    {SystemPrompt, UserPrompt}.
