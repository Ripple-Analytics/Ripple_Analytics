%%%-------------------------------------------------------------------
%%% @doc chaos_engine Helper Module - Part 3
%%% @end
%%%-------------------------------------------------------------------
-module(chaos_engine_part3).

-export([do_attack/2, do_attack/2, format_result/2, format_result/2, check_recovery/3, check_recovery/2, count_recoveries/1, calculate_recovery_rate/1, calculate_recovery_rate/2, load_history/0, save_history/1, write_status/1, format_datetime/1, format_datetime/6]).

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

