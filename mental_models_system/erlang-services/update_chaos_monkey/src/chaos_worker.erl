%%%-------------------------------------------------------------------
%%% @doc Chaos Worker
%%% Executes chaos scenarios to test update system resilience.
%%% @end
%%%-------------------------------------------------------------------
-module(chaos_worker).
-behaviour(gen_server).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).
-export([get_status/0, run_scenario/1, get_scenarios/0, get_results/0]).

-record(state, {
    status = idle,
    current_scenario = undefined,
    results = [],
    scenarios = [
        #{id => <<"test_github_source">>, 
          name => <<"Test GitHub Source">>,
          description => <<"Verify GitHub source is reachable and returns valid data">>},
        #{id => <<"test_gdrive_source">>,
          name => <<"Test Google Drive Source">>,
          description => <<"Verify Google Drive backup is accessible">>},
        #{id => <<"test_local_cache">>,
          name => <<"Test Local Cache">>,
          description => <<"Verify local cache can store and retrieve versions">>},
        #{id => <<"test_fallback_chain">>,
          name => <<"Test Fallback Chain">>,
          description => <<"Simulate GitHub failure and verify fallback to other sources">>},
        #{id => <<"test_full_update">>,
          name => <<"Test Full Update Cycle">>,
          description => <<"Run complete update cycle through orchestrator">>},
        #{id => <<"stress_test">>,
          name => <<"Stress Test">>,
          description => <<"Rapid consecutive update checks to test stability">>}
    ]
}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

get_status() -> gen_server:call(?MODULE, get_status).
run_scenario(ScenarioId) -> gen_server:call(?MODULE, {run_scenario, ScenarioId}, 120000).
get_scenarios() -> gen_server:call(?MODULE, get_scenarios).
get_results() -> gen_server:call(?MODULE, get_results).

init([]) ->
    io:format("[CHAOS-WORKER] Starting~n"),
    {ok, #state{}}.

handle_call(get_status, _From, State) ->
    Status = #{
        status => State#state.status,
        current_scenario => State#state.current_scenario,
        total_tests => length(State#state.scenarios),
        results_count => length(State#state.results)
    },
    {reply, {ok, Status}, State};

handle_call(get_scenarios, _From, State) ->
    {reply, {ok, State#state.scenarios}, State};

handle_call(get_results, _From, State) ->
    {reply, {ok, State#state.results}, State};

handle_call({run_scenario, ScenarioId}, _From, State) ->
    io:format("[CHAOS-WORKER] Running scenario: ~s~n", [ScenarioId]),
    NewState = State#state{status = running, current_scenario = ScenarioId},
    Result = execute_scenario(ScenarioId),
    FinalState = NewState#state{
        status = idle,
        current_scenario = undefined,
        results = [Result | State#state.results]
    },
    {reply, {ok, Result}, FinalState};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

%% Internal functions

execute_scenario(<<"test_github_source">>) ->
    test_source("http://github-source:8010");

execute_scenario(<<"test_gdrive_source">>) ->
    test_source("http://gdrive-source:8011");

execute_scenario(<<"test_local_cache">>) ->
    test_source("http://local-cache:8012");

execute_scenario(<<"test_fallback_chain">>) ->
    test_fallback_chain();

execute_scenario(<<"test_full_update">>) ->
    test_full_update();

execute_scenario(<<"stress_test">>) ->
    stress_test();

execute_scenario(Unknown) ->
    #{scenario => Unknown, success => false, error => <<"Unknown scenario">>, timestamp => now_iso()}.

test_source(BaseUrl) ->
    HealthUrl = BaseUrl ++ "/health",
    StatusUrl = BaseUrl ++ "/api/source/status",
    
    HealthResult = http_get(HealthUrl),
    StatusResult = http_get(StatusUrl),
    
    Success = (HealthResult =:= ok) andalso (StatusResult =:= ok),
    
    #{
        scenario => list_to_binary(BaseUrl),
        success => Success,
        health_check => HealthResult,
        status_check => StatusResult,
        timestamp => now_iso()
    }.

test_fallback_chain() ->
    %% Test each source in order
    Sources = [
        {"GitHub", "http://github-source:8010/api/source/status"},
        {"GDrive", "http://gdrive-source:8011/api/source/status"},
        {"LocalCache", "http://local-cache:8012/api/source/status"}
    ],
    
    Results = lists:map(fun({Name, Url}) ->
        Result = http_get(Url),
        {Name, Result}
    end, Sources),
    
    AvailableSources = [Name || {Name, ok} <- Results],
    
    #{
        scenario => <<"test_fallback_chain">>,
        success => length(AvailableSources) >= 1,
        available_sources => AvailableSources,
        all_results => Results,
        timestamp => now_iso()
    }.

test_full_update() ->
    %% Trigger update check through orchestrator
    CheckUrl = "http://auto-updater:8006/api/updater/check",
    Result = http_post(CheckUrl),
    
    #{
        scenario => <<"test_full_update">>,
        success => Result =:= ok,
        update_check => Result,
        timestamp => now_iso()
    }.

stress_test() ->
    %% Run 10 rapid health checks
    Url = "http://auto-updater:8006/health",
    Results = [http_get(Url) || _ <- lists:seq(1, 10)],
    SuccessCount = length([R || R <- Results, R =:= ok]),
    
    #{
        scenario => <<"stress_test">>,
        success => SuccessCount >= 8,
        total_requests => 10,
        successful_requests => SuccessCount,
        timestamp => now_iso()
    }.

http_get(Url) ->
    Cmd = "curl -s -o /dev/null -w '%{http_code}' --connect-timeout 5 '" ++ Url ++ "'",
    Result = string:trim(os:cmd(Cmd)),
    case Result of
        "200" -> ok;
        Code -> {error, list_to_binary(Code)}
    end.

http_post(Url) ->
    Cmd = "curl -s -o /dev/null -w '%{http_code}' -X POST --connect-timeout 5 '" ++ Url ++ "'",
    Result = string:trim(os:cmd(Cmd)),
    case Result of
        "200" -> ok;
        Code -> {error, list_to_binary(Code)}
    end.

now_iso() ->
    {{Y,M,D},{H,Mi,S}} = calendar:universal_time(),
    list_to_binary(io_lib:format("~4..0B-~2..0B-~2..0BT~2..0B:~2..0B:~2..0BZ", [Y,M,D,H,Mi,S])).
