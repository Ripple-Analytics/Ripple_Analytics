%%%-------------------------------------------------------------------
%%% @doc Chaos Engine
%%% 
%%% Core chaos testing engine that executes various attack types.
%%% @end
%%%-------------------------------------------------------------------
-module(chaos_engine).
-behaviour(gen_server).

-export([start_link/0, execute_attack/1, get_status/0, enable/0, disable/0, 
         set_config/1, get_history/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-define(SERVER, ?MODULE).
-define(STATUS_FILE, "/data/chaos_status.json").
-define(HISTORY_FILE, "/data/chaos_history.json").
-define(MAX_HISTORY, 100).

-record(state, {
    enabled :: boolean(),
    target_services :: [string()],
    attack_history :: list(),
    last_attack :: undefined | calendar:datetime(),
    total_attacks :: non_neg_integer(),
    successful_recoveries :: non_neg_integer()
}).

-type attack_type() :: kill_container | network_latency | cpu_stress | 
                       memory_pressure | disk_fill | service_restart.

%%====================================================================
%% API
%%====================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

execute_attack(AttackType) ->
    gen_server:call(?SERVER, {execute_attack, AttackType}, 60000).

get_status() ->
    gen_server:call(?SERVER, get_status).

enable() ->
    gen_server:call(?SERVER, enable).

disable() ->
    gen_server:call(?SERVER, disable).

set_config(Config) ->
    gen_server:call(?SERVER, {set_config, Config}).

get_history() ->
    gen_server:call(?SERVER, get_history).

%%====================================================================
%% gen_server callbacks
%%====================================================================

init([]) ->
    Enabled = application:get_env(chaos_monkey, enabled, false),
    Targets = application:get_env(chaos_monkey, target_services, []),
    
    %% Load history from file
    History = load_history(),
    
    State = #state{
        enabled = Enabled,
        target_services = Targets,
        attack_history = History,
        last_attack = undefined,
        total_attacks = length(History),
        successful_recoveries = count_recoveries(History)
    },
    
    write_status(State),
    {ok, State}.

handle_call({execute_attack, AttackType}, _From, State) ->
    case State#state.enabled of
        false ->
            {reply, {error, chaos_disabled}, State};
        true ->
            Result = do_attack(AttackType, State#state.target_services),
            
            %% Record attack in history
            AttackRecord = #{
                <<"type">> => atom_to_binary(AttackType),
                <<"timestamp">> => format_datetime(calendar:local_time()),
                <<"result">> => format_result(Result),
                <<"recovered">> => check_recovery(AttackType, Result)
            },
            
            NewHistory = [AttackRecord | lists:sublist(State#state.attack_history, ?MAX_HISTORY - 1)],
            
            NewState = State#state{
                attack_history = NewHistory,
                last_attack = calendar:local_time(),
                total_attacks = State#state.total_attacks + 1,
                successful_recoveries = State#state.successful_recoveries + 
                    case maps:get(<<"recovered">>, AttackRecord) of true -> 1; false -> 0 end
            },
            
            save_history(NewHistory),
            write_status(NewState),
            
            {reply, Result, NewState}
    end;

handle_call(get_status, _From, State) ->
    Status = #{
        <<"enabled">> => State#state.enabled,
        <<"target_services">> => [list_to_binary(S) || S <- State#state.target_services],
        <<"last_attack">> => format_datetime(State#state.last_attack),
        <<"total_attacks">> => State#state.total_attacks,
        <<"successful_recoveries">> => State#state.successful_recoveries,
        <<"recovery_rate">> => calculate_recovery_rate(State)
    },
    {reply, {ok, Status}, State};

handle_call(enable, _From, State) ->
    NewState = State#state{enabled = true},
    write_status(NewState),
    io:format("[Chaos Monkey] ENABLED - Attacks will now be executed~n"),
    {reply, ok, NewState};

handle_call(disable, _From, State) ->
    NewState = State#state{enabled = false},
    write_status(NewState),
    io:format("[Chaos Monkey] DISABLED - No attacks will be executed~n"),
    {reply, ok, NewState};

handle_call({set_config, Config}, _From, State) ->
    Targets = case maps:get(<<"target_services">>, Config, undefined) of
        undefined -> State#state.target_services;
        T when is_list(T) -> [binary_to_list(S) || S <- T]
    end,
    
    NewState = State#state{target_services = Targets},
    write_status(NewState),
    {reply, ok, NewState};

handle_call(get_history, _From, State) ->
    {reply, {ok, State#state.attack_history}, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

%%====================================================================
%% Attack implementations
%%====================================================================

-spec do_attack(attack_type(), [string()]) -> {ok, map()} | {error, term()}.

do_attack(kill_container, Targets) ->
    %% Randomly select a target container and kill it
    case Targets of
        [] -> {error, no_targets};
        _ ->
            Target = lists:nth(rand:uniform(length(Targets)), Targets),
            io:format("[Chaos Monkey] Killing container: ~s~n", [Target]),
            
            Cmd = io_lib:format("docker kill ~s 2>&1 || true", [Target]),
            Output = os:cmd(Cmd),
            
            %% Wait a moment then check if it recovered (supervisor should restart it)
            timer:sleep(5000),
            RecoverCmd = io_lib:format("docker ps --filter name=~s --format '{{.Status}}' 2>&1", [Target]),
            RecoverOutput = os:cmd(RecoverCmd),
            
            Recovered = string:find(RecoverOutput, "Up") =/= nomatch,
            
            {ok, #{
                <<"attack">> => <<"kill_container">>,
                <<"target">> => list_to_binary(Target),
                <<"output">> => list_to_binary(Output),
                <<"recovered">> => Recovered
            }}
    end;

do_attack(network_latency, Targets) ->
    %% Inject network latency using tc (traffic control)
    case Targets of
        [] -> {error, no_targets};
        _ ->
            Target = lists:nth(rand:uniform(length(Targets)), Targets),
            Latency = 100 + rand:uniform(400), %% 100-500ms
            Duration = 10, %% seconds
            
            io:format("[Chaos Monkey] Injecting ~pms latency to ~s for ~ps~n", [Latency, Target, Duration]),
            
            %% Note: This requires NET_ADMIN capability in Docker
            Cmd = io_lib:format(
                "docker exec ~s sh -c 'tc qdisc add dev eth0 root netem delay ~pms 2>&1 || echo latency_injected' && "
                "sleep ~p && "
                "docker exec ~s sh -c 'tc qdisc del dev eth0 root 2>&1 || true'",
                [Target, Latency, Duration, Target]),
            Output = os:cmd(Cmd),
            
            {ok, #{
                <<"attack">> => <<"network_latency">>,
                <<"target">> => list_to_binary(Target),
                <<"latency_ms">> => Latency,
                <<"duration_s">> => Duration,
                <<"output">> => list_to_binary(Output)
            }}
    end;

do_attack(cpu_stress, Targets) ->
    %% Stress CPU in a container
    case Targets of
        [] -> {error, no_targets};
        _ ->
            Target = lists:nth(rand:uniform(length(Targets)), Targets),
            Duration = 10, %% seconds
            
            io:format("[Chaos Monkey] Stressing CPU in ~s for ~ps~n", [Target, Duration]),
            
            Cmd = io_lib:format(
                "docker exec ~s sh -c 'timeout ~p dd if=/dev/zero of=/dev/null 2>&1 &' || true",
                [Target, Duration]),
            Output = os:cmd(Cmd),
            
            {ok, #{
                <<"attack">> => <<"cpu_stress">>,
                <<"target">> => list_to_binary(Target),
                <<"duration_s">> => Duration,
                <<"output">> => list_to_binary(Output)
            }}
    end;

do_attack(memory_pressure, Targets) ->
    %% Create memory pressure in a container
    case Targets of
        [] -> {error, no_targets};
        _ ->
            Target = lists:nth(rand:uniform(length(Targets)), Targets),
            SizeMB = 50 + rand:uniform(100), %% 50-150MB
            Duration = 10,
            
            io:format("[Chaos Monkey] Creating ~pMB memory pressure in ~s~n", [SizeMB, Target]),
            
            Cmd = io_lib:format(
                "docker exec ~s sh -c 'head -c ~pm /dev/zero | tail &' && sleep ~p && "
                "docker exec ~s sh -c 'pkill -f \"head -c\" 2>/dev/null || true'",
                [Target, SizeMB, Duration, Target]),
            Output = os:cmd(Cmd),
            
            {ok, #{
                <<"attack">> => <<"memory_pressure">>,
                <<"target">> => list_to_binary(Target),
                <<"size_mb">> => SizeMB,
                <<"duration_s">> => Duration,
                <<"output">> => list_to_binary(Output)
            }}
    end;

do_attack(service_restart, Targets) ->
    %% Gracefully restart a service
    case Targets of
        [] -> {error, no_targets};
        _ ->
            Target = lists:nth(rand:uniform(length(Targets)), Targets),
            
            io:format("[Chaos Monkey] Restarting service: ~s~n", [Target]),
            
            Cmd = io_lib:format("docker restart ~s 2>&1", [Target]),
            Output = os:cmd(Cmd),
            
            %% Check if it came back up
            timer:sleep(10000),
            CheckCmd = io_lib:format("docker ps --filter name=~s --format '{{.Status}}' 2>&1", [Target]),
            CheckOutput = os:cmd(CheckCmd),
            
            Recovered = string:find(CheckOutput, "Up") =/= nomatch,
            
            {ok, #{
                <<"attack">> => <<"service_restart">>,
                <<"target">> => list_to_binary(Target),
                <<"output">> => list_to_binary(Output),
                <<"recovered">> => Recovered
            }}
    end;

do_attack(random, Targets) ->
    %% Execute a random attack type
    AttackTypes = [kill_container, network_latency, cpu_stress, memory_pressure, service_restart],
    RandomAttack = lists:nth(rand:uniform(length(AttackTypes)), AttackTypes),
    do_attack(RandomAttack, Targets);

do_attack(_, _) ->
    {error, unknown_attack_type}.

%%====================================================================
%% Internal functions
%%====================================================================

format_result({ok, Data}) -> Data;
format_result({error, Reason}) -> #{<<"error">> => list_to_binary(io_lib:format("~p", [Reason]))}.

check_recovery(_AttackType, {ok, #{<<"recovered">> := Recovered}}) -> Recovered;
check_recovery(_, _) -> false.

count_recoveries(History) ->
    length([H || H <- History, maps:get(<<"recovered">>, H, false) =:= true]).

calculate_recovery_rate(#state{total_attacks = 0}) -> 100.0;
calculate_recovery_rate(#state{total_attacks = Total, successful_recoveries = Recovered}) ->
    (Recovered / Total) * 100.

load_history() ->
    case file:read_file(?HISTORY_FILE) of
        {ok, Content} ->
            try jsx:decode(Content, [return_maps]) catch _:_ -> [] end;
        {error, _} ->
            []
    end.

save_history(History) ->
    filelib:ensure_dir(?HISTORY_FILE),
    file:write_file(?HISTORY_FILE, jsx:encode(History)).

write_status(State) ->
    Status = #{
        <<"enabled">> => State#state.enabled,
        <<"target_services">> => [list_to_binary(S) || S <- State#state.target_services],
        <<"last_attack">> => format_datetime(State#state.last_attack),
        <<"total_attacks">> => State#state.total_attacks,
        <<"successful_recoveries">> => State#state.successful_recoveries,
        <<"recovery_rate">> => calculate_recovery_rate(State),
        <<"updated_at">> => format_datetime(calendar:local_time())
    },
    filelib:ensure_dir(?STATUS_FILE),
    file:write_file(?STATUS_FILE, jsx:encode(Status)).

format_datetime(undefined) -> <<"never">>;
format_datetime({{Y, M, D}, {H, Mi, S}}) ->
    list_to_binary(io_lib:format("~4..0B-~2..0B-~2..0BT~2..0B:~2..0B:~2..0B", [Y, M, D, H, Mi, S])).
