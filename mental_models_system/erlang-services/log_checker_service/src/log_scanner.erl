%%%-------------------------------------------------------------------
%%% @doc Log Scanner
%%% Continuously monitors docker logs from all services.
%%% Detects errors, warnings, and failures in real-time.
%%% @end
%%%-------------------------------------------------------------------
-module(log_scanner).
-behaviour(gen_server).

-export([start_link/0, init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).
-export([get_status/0, scan_now/0, get_recent_errors/0]).

-record(state, {
    scan_interval = 30000,  %% 30 seconds
    timer_ref = undefined,
    last_scan = undefined,
    services = [],
    error_count = 0,
    scan_count = 0
}).

-define(SERVICES, [
    "mental-models-desktop-ui-blue",
    "mental-models-desktop-ui-green",
    "mental-models-analysis-service-blue",
    "mental-models-analysis-service-green",
    "mental-models-api-gateway-blue",
    "mental-models-api-gateway-green",
    "mental-models-storage-service-blue",
    "mental-models-storage-service-green",
    "mental-models-auto-updater",
    "mental-models-lm-studio-blue",
    "mental-models-lm-studio-green",
    "mental-models-log-checker-blue",
    "mental-models-log-checker-green"
]).

%%====================================================================
%% API
%%====================================================================

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

get_status() ->
    gen_server:call(?MODULE, get_status).

scan_now() ->
    gen_server:cast(?MODULE, scan_now).

get_recent_errors() ->
    gen_server:call(?MODULE, get_recent_errors).

%%====================================================================
%% gen_server callbacks
%%====================================================================

init([]) ->
    io:format("[LOG-SCANNER] Starting log scanner~n"),
    io:format("[LOG-SCANNER] Monitoring ~p services~n", [length(?SERVICES)]),
    
    %% Start first scan after 5 seconds
    TimerRef = erlang:send_after(5000, self(), scan_timer),
    
    {ok, #state{timer_ref = TimerRef, services = ?SERVICES}}.

handle_call(get_status, _From, State) ->
    Status = #{
        scan_interval => State#state.scan_interval,
        last_scan => State#state.last_scan,
        services_monitored => length(State#state.services),
        error_count => State#state.error_count,
        scan_count => State#state.scan_count
    },
    {reply, {ok, Status}, State};

handle_call(get_recent_errors, _From, State) ->
    %% Get errors from error_aggregator
    Errors = case catch error_aggregator:get_errors() of
        {ok, E} -> E;
        _ -> []
    end,
    {reply, {ok, Errors}, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown}, State}.

handle_cast(scan_now, State) ->
    NewState = do_scan(State),
    {noreply, NewState};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(scan_timer, State) ->
    NewState = do_scan(State),
    TimerRef = erlang:send_after(State#state.scan_interval, self(), scan_timer),
    {noreply, NewState#state{timer_ref = TimerRef}};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================

do_scan(State) ->
    io:format("[LOG-SCANNER] Scanning logs (~p)...~n", [State#state.scan_count + 1]),
    
    %% Scan each service
    Errors = lists:foldl(fun(Service, Acc) ->
        case scan_service_logs(Service) of
            {ok, []} -> Acc;
            {ok, ServiceErrors} -> Acc ++ ServiceErrors;
            {error, _} -> Acc
        end
    end, [], State#state.services),
    
    %% Send errors to aggregator
    case Errors of
        [] -> ok;
        _ ->
            io:format("[LOG-SCANNER] Found ~p errors~n", [length(Errors)]),
            lists:foreach(fun(Error) ->
                error_aggregator:add_error(Error)
            end, Errors)
    end,
    
    State#state{
        last_scan = erlang:timestamp(),
        error_count = State#state.error_count + length(Errors),
        scan_count = State#state.scan_count + 1
    }.

scan_service_logs(Service) ->
    %% Get last 100 lines of logs from the service
    Cmd = "docker logs --tail 100 " ++ Service ++ " 2>&1",
    Output = os:cmd(Cmd),
    
    %% Parse for errors
    Lines = string:split(Output, "\n", all),
    Errors = lists:filtermap(fun(Line) ->
        case is_error_line(Line) of
            true -> {true, #{
                service => list_to_binary(Service),
                line => list_to_binary(Line),
                timestamp => erlang:timestamp(),
                type => classify_error(Line)
            }};
            false -> false
        end
    end, Lines),
    
    {ok, Errors}.

is_error_line(Line) ->
    LowerLine = string:lowercase(Line),
    lists:any(fun(Pattern) ->
        string:find(LowerLine, Pattern) =/= nomatch
    end, [
        "error",
        "exception",
        "failed",
        "crash",
        "badmatch",
        "badarg",
        "undef",
        "badarith",
        "function_clause",
        "case_clause",
        "if_clause",
        "noproc",
        "timeout",
        "econnrefused",
        "compile error",
        "syntax error"
    ]).

classify_error(Line) ->
    LowerLine = string:lowercase(Line),
    case true of
        _ when string:find(LowerLine, "compile") =/= nomatch -> <<"compile_error">>;
        _ when string:find(LowerLine, "syntax") =/= nomatch -> <<"syntax_error">>;
        _ when string:find(LowerLine, "crash") =/= nomatch -> <<"crash">>;
        _ when string:find(LowerLine, "timeout") =/= nomatch -> <<"timeout">>;
        _ when string:find(LowerLine, "connection") =/= nomatch -> <<"connection_error">>;
        _ when string:find(LowerLine, "badmatch") =/= nomatch -> <<"runtime_error">>;
        _ when string:find(LowerLine, "undef") =/= nomatch -> <<"undefined_function">>;
        _ -> <<"general_error">>
    end.
