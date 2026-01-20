%%%-------------------------------------------------------------------
%%% @doc chaos_engine Helper Module - Part 2
%%% @end
%%%-------------------------------------------------------------------
-module(chaos_engine_part2).

-export([handle_cast/2, handle_info/2, terminate/2, do_attack/2, do_attack/2, do_attack/2, do_attack/2, do_attack/2]).

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

