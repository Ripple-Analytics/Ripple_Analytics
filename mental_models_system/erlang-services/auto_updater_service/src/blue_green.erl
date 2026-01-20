%%%-------------------------------------------------------------------
%%% @doc Blue-Green Deployment - Traffic switching and service management
%%% With verbose GitHub logging for remote debugging
%%% @end
%%%-------------------------------------------------------------------
-module(blue_green).

-export([get_standby_services/1, switch_traffic/1, read_active_env/0]).
-export([build_standby/1, start_standby/1, check_standby_health/1]).

-define(DATA_DIR, "/data").
-define(BASE_PATH, "/repo/mental_models_system/erlang-services").

%% @doc Get list of standby service names
%% Includes debug-logger for blue-green updates
get_standby_services(Env) ->
    Services = ["debug-logger-" ++ Env, "desktop-ui-" ++ Env, "analysis-service-" ++ Env],
    github_logger:log("blue_green", "get_services", 
        lists:flatten(io_lib:format("Standby env: ~s~nServices: ~p", [Env, Services]))),
    Services.

%% @doc Build all standby services with logging
build_standby(Services) ->
    github_logger:log("blue_green", "build_start",
        lists:flatten(io_lib:format("Starting build for: ~p", [Services]))),
    Result = build_services(Services),
    github_logger:log("blue_green", "build_end",
        lists:flatten(io_lib:format("Build result: ~p", [Result]))),
    Result.

build_services([]) -> ok;
build_services([S | Rest]) ->
    io:format("[BLUE-GREEN] Building service: ~s~n", [S]),
    case docker_utils:build_service(?BASE_PATH, S) of
        ok -> build_services(Rest);
        Err -> 
            github_logger:log_error("blue_green", S, 
                lists:flatten(io_lib:format("Build failed: ~p", [Err]))),
            Err
    end.

%% @doc Start standby services with logging
start_standby(Services) ->
    io:format("[BLUE-GREEN] Starting services: ~p~n", [Services]),
    github_logger:log("blue_green", "start_services",
        lists:flatten(io_lib:format("Starting: ~p", [Services]))),
    docker_utils:start_services(?BASE_PATH, Services).

%% @doc Check if standby is healthy with logging
check_standby_health(Env) ->
    Container = "mental-models-ui-" ++ Env,
    io:format("[BLUE-GREEN] Health check: ~s~n", [Container]),
    github_logger:log("blue_green", "health_check_start",
        lists:flatten(io_lib:format("Checking health of: ~s", [Container]))),
    Result = check_health_with_retry(Container, 3),
    github_logger:log("blue_green", "health_check_end",
        lists:flatten(io_lib:format("Health result for ~s: ~p", [Container, Result]))),
    Result.

check_health_with_retry(_, 0) -> 
    github_logger:log_error("blue_green", "health", "Health check failed after 3 retries"),
    false;
check_health_with_retry(Container, Retries) ->
    case docker_utils:check_health(Container) of
        true -> true;
        false ->
            github_logger:log("blue_green", "health_retry",
                lists:flatten(io_lib:format("Retry ~p for ~s", [4 - Retries, Container]))),
            timer:sleep(5000),
            check_health_with_retry(Container, Retries - 1)
    end.

%% @doc Switch traffic to new environment with logging
switch_traffic(NewEnv) ->
    io:format("[BLUE-GREEN] Switching traffic to: ~s~n", [NewEnv]),
    github_logger:log("blue_green", "switch_traffic",
        lists:flatten(io_lib:format("Switching to: ~s", [NewEnv]))),
    
    file_utils:write(?DATA_DIR ++ "/active_env", NewEnv),
    update_nginx_configs(NewEnv),
    reload_nginx(),
    
    github_logger:log_deploy("traffic_switch", "success",
        lists:flatten(io_lib:format("Now serving from: ~s", [NewEnv]))).

%% @doc Read current active environment
read_active_env() ->
    case file_utils:read(?DATA_DIR ++ "/active_env") of
        undefined -> "blue";
        Env when is_binary(Env) -> binary_to_list(Env);
        Env when is_list(Env) -> Env
    end.

%% Internal: update nginx configs inside proxy container
update_nginx_configs(Env) ->
    ProxyContainer = "mental-models-proxy",
    Upstream = "ui-" ++ Env,
    Config = lists:flatten(io_lib:format(
        "upstream active_backend { server ~s:3000; }\\n"
        "server {\\n"
        "    listen 80;\\n"
        "    location / {\\n"
        "        proxy_pass http://active_backend;\\n"
        "        proxy_http_version 1.1;\\n"
        "        proxy_set_header Upgrade \\$http_upgrade;\\n"
        "        proxy_set_header Connection upgrade;\\n"
        "        proxy_set_header Host \\$host;\\n"
        "    }\\n"
        "}\\n", [Upstream])),
    WriteCmd = "printf '" ++ Config ++ "' > /etc/nginx/conf.d/default.conf",
    Result = docker_utils:exec_cmd(ProxyContainer, WriteCmd),
    github_logger:log("blue_green", "nginx_config",
        lists:flatten(io_lib:format("Updated nginx for ~s: ~s", [Env, Result]))).

%% Internal: reload nginx
reload_nginx() ->
    ProxyContainer = "mental-models-proxy",
    Result = docker_utils:exec_cmd(ProxyContainer, "nginx -s reload"),
    io:format("[BLUE-GREEN] Nginx reload: ~s~n", [lists:flatten(Result)]),
    github_logger:log("blue_green", "nginx_reload", Result).
