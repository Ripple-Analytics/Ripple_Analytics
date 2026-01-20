%%%-------------------------------------------------------------------
%%% @doc Blue-Green Deployment - Traffic switching and service management
%%% @end
%%%-------------------------------------------------------------------
-module(blue_green).

-export([get_standby_services/1, switch_traffic/1, read_active_env/0]).
-export([build_standby/1, start_standby/1, check_standby_health/1]).

-define(DATA_DIR, "/data").
-define(BASE_PATH, "/repo/mental_models_system/erlang-services").

%% @doc Get list of standby service names
get_standby_services(Env) ->
    ["desktop-ui-" ++ Env, "analysis-service-" ++ Env].

%% @doc Build all standby services
build_standby(Services) ->
    build_services(Services).

build_services([]) -> ok;
build_services([S | Rest]) ->
    io:format("[BLUE-GREEN] Building service: ~s~n", [S]),
    case docker_utils:build_service(?BASE_PATH, S) of
        ok -> build_services(Rest);
        Err -> Err
    end.

%% @doc Start standby services
start_standby(Services) ->
    io:format("[BLUE-GREEN] Starting services: ~p~n", [Services]),
    docker_utils:start_services(?BASE_PATH, Services).

%% @doc Check if standby is healthy
check_standby_health(Env) ->
    Container = "mental-models-ui-" ++ Env,
    io:format("[BLUE-GREEN] Health check: ~s~n", [Container]),
    %% Try multiple times with delay
    check_health_with_retry(Container, 3).

check_health_with_retry(_, 0) -> false;
check_health_with_retry(Container, Retries) ->
    case docker_utils:check_health(Container) of
        true -> true;
        false ->
            timer:sleep(5000),
            check_health_with_retry(Container, Retries - 1)
    end.

%% @doc Switch traffic to new environment
switch_traffic(NewEnv) ->
    io:format("[BLUE-GREEN] Switching traffic to: ~s~n", [NewEnv]),
    file_utils:write(?DATA_DIR ++ "/active_env", NewEnv),
    update_nginx_configs(NewEnv),
    reload_nginx().

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
    %% Note: Using ~~ to escape ~ in format string, and proper escaping for shell
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
    docker_utils:exec_cmd(ProxyContainer, WriteCmd).

%% Internal: reload nginx
reload_nginx() ->
    ProxyContainer = "mental-models-proxy",
    Result = docker_utils:exec_cmd(ProxyContainer, "nginx -s reload"),
    io:format("[BLUE-GREEN] Nginx reload: ~s~n", [lists:flatten(Result)]).
