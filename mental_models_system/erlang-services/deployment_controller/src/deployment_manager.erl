-module(deployment_manager).
-behaviour(gen_server).
-export([start_link/0, init/1, handle_call/3, handle_cast/2, handle_info/2]).
-export([get_status/0, build_standby/1, switch_environment/0, rollback/0]).

-record(state, {
    active_env = "blue",
    standby_env = "green",
    last_build = undefined,
    last_switch = undefined,
    build_in_progress = false
}).

start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

get_status() -> gen_server:call(?MODULE, get_status).
build_standby(CommitHash) -> gen_server:call(?MODULE, {build_standby, CommitHash}, 600000).
switch_environment() -> gen_server:call(?MODULE, switch_environment, 120000).
rollback() -> gen_server:call(?MODULE, rollback, 120000).

init([]) ->
    ActiveEnv = read_active_env(),
    StandbyEnv = case ActiveEnv of "blue" -> "green"; _ -> "blue" end,
    io:format("[DEPLOY-MGR] Active: ~s, Standby: ~s~n", [ActiveEnv, StandbyEnv]),
    {ok, #state{active_env = ActiveEnv, standby_env = StandbyEnv}}.

handle_call(get_status, _From, State) ->
    Status = #{
        active_env => list_to_binary(State#state.active_env),
        standby_env => list_to_binary(State#state.standby_env),
        build_in_progress => State#state.build_in_progress,
        last_build => State#state.last_build,
        last_switch => State#state.last_switch
    },
    {reply, {ok, Status}, State};

handle_call({build_standby, CommitHash}, _From, State) ->
    io:format("[DEPLOY-MGR] Building standby (~s) with commit ~s~n", 
              [State#state.standby_env, CommitHash]),
    Result = do_build_standby(State#state.standby_env, CommitHash),
    NewState = State#state{
        build_in_progress = false,
        last_build = erlang:timestamp()
    },
    {reply, Result, NewState};

handle_call(switch_environment, _From, State) ->
    io:format("[DEPLOY-MGR] Switching from ~s to ~s~n", 
              [State#state.active_env, State#state.standby_env]),
    case do_switch(State#state.standby_env) of
        ok ->
            NewState = State#state{
                active_env = State#state.standby_env,
                standby_env = State#state.active_env,
                last_switch = erlang:timestamp()
            },
            {reply, ok, NewState};
        Error ->
            {reply, Error, State}
    end;

handle_call(rollback, _From, State) ->
    io:format("[DEPLOY-MGR] Rolling back to ~s~n", [State#state.standby_env]),
    case do_switch(State#state.standby_env) of
        ok ->
            NewState = State#state{
                active_env = State#state.standby_env,
                standby_env = State#state.active_env
            },
            {reply, ok, NewState};
        Error ->
            {reply, Error, State}
    end;

handle_call(_Request, _From, State) -> {reply, {error, unknown}, State}.
handle_cast(_Msg, State) -> {noreply, State}.
handle_info(_Info, State) -> {noreply, State}.

read_active_env() ->
    case file:read_file("/data/active_env") of
        {ok, Bin} -> string:trim(binary_to_list(Bin));
        _ -> "blue"
    end.

do_build_standby(Env, CommitHash) ->
    BasePath = "/repo/mental_models_system/erlang-services",
    BuildTime = calendar:system_time_to_rfc3339(erlang:system_time(second)),
    Services = get_standby_services(Env),
    BuildArgs = "--build-arg COMMIT_HASH=" ++ CommitHash ++ " --build-arg BUILD_TIME=" ++ BuildTime,
    Cmd = "cd " ++ BasePath ++ " && docker-compose build --no-cache " ++ BuildArgs ++ " " ++ Services ++ " 2>&1",
    io:format("[DEPLOY-MGR] Build command: ~s~n", [Cmd]),
    os:cmd(Cmd),
    StartCmd = "cd " ++ BasePath ++ " && docker-compose up -d " ++ Services ++ " 2>&1",
    os:cmd(StartCmd),
    timer:sleep(15000),
    ok.

get_standby_services(Env) ->
    lists:flatten([
        "desktop-ui-" ++ Env ++ " ",
        "api-gateway-" ++ Env ++ " ",
        "analysis-service-" ++ Env ++ " ",
        "harvester-service-" ++ Env ++ " ",
        "storage-service-" ++ Env ++ " ",
        "chaos-engineering-" ++ Env ++ " ",
        "auto-updater-" ++ Env ++ " ",
        "deployment-controller-" ++ Env
    ]).

do_switch(NewEnv) ->
    file:write_file("/data/active_env", NewEnv),
    BasePath = "/repo/mental_models_system/erlang-services",
    ActiveConf = generate_active_conf(NewEnv),
    file:write_file(BasePath ++ "/nginx_proxy/active_env/active.conf", ActiveConf),
    os:cmd("docker exec mental-models-proxy nginx -s reload 2>&1"),
    io:format("[DEPLOY-MGR] Switched to ~s~n", [NewEnv]),
    ok.

generate_active_conf(Env) ->
    lists:flatten(io_lib:format(
        "# Active: ~s~nset $active_env ~s;~n", [Env, Env])).
