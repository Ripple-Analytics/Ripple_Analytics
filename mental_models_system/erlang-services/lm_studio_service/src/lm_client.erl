%%%-------------------------------------------------------------------
%%% @doc LM Client - Robust LLM connection with retry and fallback
%%% 
%%% Connects to LM Studio on the host machine and provides
%%% reliable LLM generation with automatic retry and error handling.
%%% @end
%%%-------------------------------------------------------------------
-module(lm_client).
%% Helper modules: lm_client_part2
-behaviour(gen_server).

-export([start_link/0, generate/2, generate/3, check_connection/0, get_status/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-record(state, {
    base_url :: string(),
    connected :: boolean(),
    last_check :: integer(),
    total_requests :: integer(),
    successful_requests :: integer(),
    failed_requests :: integer(),
    avg_response_time :: float()
}).

-define(DEFAULT_TIMEOUT, 120000).
-define(RETRY_ATTEMPTS, 3).
-define(RETRY_DELAY, 1000).

%%====================================================================
%% API
%%====================================================================

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

generate(Prompt, SystemPrompt) ->
    generate(Prompt, SystemPrompt, #{}).

generate(Prompt, SystemPrompt, Options) ->
    gen_server:call(?MODULE, {generate, Prompt, SystemPrompt, Options}, ?DEFAULT_TIMEOUT).

check_connection() ->
    gen_server:call(?MODULE, check_connection, 10000).

get_status() ->
    gen_server:call(?MODULE, get_status).

%%====================================================================
%% gen_server callbacks
%%====================================================================

init([]) ->
    BaseUrl = get_lm_studio_url(),
    io:format("[LM-CLIENT] Initializing with base URL: ~s~n", [BaseUrl]),
    
    %% Check connection on startup
    self() ! check_initial_connection,
    
    {ok, #state{
        base_url = BaseUrl,
        connected = false,
        last_check = 0,
        total_requests = 0,
        successful_requests = 0,
        failed_requests = 0,
        avg_response_time = 0.0
    }}.

handle_call({generate, Prompt, SystemPrompt, Options}, _From, State) ->
    StartTime = erlang:monotonic_time(millisecond),
    
    Temperature = maps:get(temperature, Options, 0.7),
    MaxTokens = maps:get(max_tokens, Options, 4096),
    Model = maps:get(model, Options, <<"local-model">>),
    
    RequestBody = jsx:encode(#{
        <<"model">> => Model,
        <<"messages">> => [
            #{<<"role">> => <<"system">>, <<"content">> => ensure_binary(SystemPrompt)},
            #{<<"role">> => <<"user">>, <<"content">> => ensure_binary(Prompt)}
        ],
        <<"temperature">> => Temperature,
        <<"max_tokens">> => MaxTokens,
        <<"stream">> => false
    }),
    
    Result = do_request_with_retry(State#state.base_url ++ "/v1/chat/completions", 
                                    RequestBody, ?RETRY_ATTEMPTS),
    
    EndTime = erlang:monotonic_time(millisecond),
    ResponseTime = EndTime - StartTime,
    
    NewState = update_stats(State, Result, ResponseTime),
    
    case Result of
        {ok, Content} ->
            io:format("[LM-CLIENT] Generation successful (~p ms)~n", [ResponseTime]),
            {reply, {ok, Content}, NewState#state{connected = true}};
        {error, Reason} ->
            io:format("[LM-CLIENT] Generation failed: ~p~n", [Reason]),
            {reply, {error, Reason}, NewState}
    end;

handle_call(check_connection, _From, State) ->
    Url = State#state.base_url ++ "/v1/models",
    Result = case hackney:request(get, list_to_binary(Url), [], <<>>, [{timeout, 5000}]) of
        {ok, 200, _Headers, ClientRef} ->
            hackney:body(ClientRef),
            {ok, connected};
        {ok, StatusCode, _Headers, _ClientRef} ->
            {error, {http_error, StatusCode}};
        {error, Reason} ->
            {error, Reason}
    end,
    
    Connected = Result =:= {ok, connected},
    io:format("[LM-CLIENT] Connection check: ~p~n", [Result]),
    
    {reply, Result, State#state{
        connected = Connected, 
        last_check = erlang:system_time(second)
    }};

handle_call(get_status, _From, State) ->
    Status = #{
        <<"connected">> => State#state.connected,
        <<"base_url">> => list_to_binary(State#state.base_url),
        <<"last_check">> => State#state.last_check,
        <<"total_requests">> => State#state.total_requests,
        <<"successful_requests">> => State#state.successful_requests,
        <<"failed_requests">> => State#state.failed_requests,
        <<"success_rate">> => calculate_success_rate(State),
        <<"avg_response_time_ms">> => State#state.avg_response_time
    },
    {reply, {ok, Status}, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.
