-module(ai_code_improver_app).
-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/health", health_handler, []},
            {"/api/improver/status", improver_status_handler, []},
            {"/api/improver/start", improver_start_handler, []},
            {"/api/improver/stop", improver_stop_handler, []},
            {"/api/improver/history", improver_history_handler, []},
            {"/api/improver/config", improver_config_handler, []},
            {"/api/improver/analyze", code_analyzer_handler, []},
            {"/api/improver/suggest", improvement_suggester_handler, []},
            {"/api/improver/validate", code_validator_handler, []},
            {"/api/improver/deploy", autonomous_deployer_handler, []},
            {"/api/prompts/philosophy", design_philosophy_handler, []},
            {"/api/prompts/templates", prompt_templates_handler, []}
        ]}
    ]),
    {ok, _} = cowboy:start_clear(ai_code_improver_http,
        [{port, 8025}],
        #{env => #{dispatch => Dispatch}}
    ),
    io:format("AI Code Improver started on port 8025~n"),
    ai_code_improver_sup:start_link().

stop(_State) ->
    ok = cowboy:stop_listener(ai_code_improver_http),
    ok.
