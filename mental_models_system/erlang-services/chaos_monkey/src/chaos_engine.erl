%%%-------------------------------------------------------------------
%%% @doc Chaos Engine
%%% 
%%% Core chaos testing engine that executes various attack types.
%%% @end
%%%-------------------------------------------------------------------
-module(chaos_engine).
%% Helper modules: chaos_engine_part2, chaos_engine_part3
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
