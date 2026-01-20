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
    case docker_utils:build_service(?BASE_PATH, S) of
        ok -> build_services(Rest);
        Err -> Err
    end.

%% @doc Start standby services
start_standby(Services) ->
    docker_utils:start_services(?BASE_PATH, Services).

%% @doc Check if standby is healthy
check_standby_health(Env) ->
    Container = "mental-models-ui-" ++ Env,
    docker_utils:check_health(Container).

%% @doc Switch traffic to new environment
switch_traffic(NewEnv) ->
    io:format("[BLUE-GREEN] Switching to: ~s~n", [NewEnv]),
    file_utils:write(?DATA_DIR ++ "/active_env", NewEnv),
    update_nginx_configs(NewEnv),
    reload_nginx().

%% @doc Read current active environment
read_active_env() ->
    case file_utils:read(?DATA_DIR ++ "/active_env") of
        undefined -> "blue";
        Env -> binary_to_list(Env)
    end.

%% Internal: update nginx configs
update_nginx_configs(Env) ->
    Configs = [{"active.conf", "ui-" ++ Env}],
    lists:foreach(fun({File, Upstream}) ->
        Content = "location / { proxy_pass http://" ++ Upstream ++ "; }",
        Cmd = "echo '" ++ Content ++ "' > /etc/nginx/conf.d/" ++ File,
        docker_utils:exec_cmd("mental-models-proxy", Cmd)
    end, Configs).

%% Internal: reload nginx
reload_nginx() ->
    docker_utils:exec_cmd("mental-models-proxy", "nginx -s reload").
