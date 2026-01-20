%%%-------------------------------------------------------------------
%%% @doc LLM Client
%%% 
%%% Handles integration with LM Studio for AI-powered analysis.
%%% Includes retry logic and fallback to keyword-based analysis.
%%% @end
%%%-------------------------------------------------------------------
-module(llm_client).
%% Helper modules: llm_client_part2, llm_client_part3
-behaviour(gen_server).

-export([start_link/0, analyze/2, detect_biases/1, is_available/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-define(SERVER, ?MODULE).
-define(TIMEOUT, 60000).
-define(MAX_RETRIES, 3).

-record(state, {
    url :: string(),
    available :: boolean()
}).

%%====================================================================
%% API
%%====================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

analyze(Text, TopN) ->
    gen_server:call(?SERVER, {analyze, Text, TopN}, ?TIMEOUT).

detect_biases(Text) ->
    gen_server:call(?SERVER, {detect_biases, Text}, ?TIMEOUT).

is_available() ->
    gen_server:call(?SERVER, is_available).

%%====================================================================
%% gen_server callbacks
%%====================================================================

init([]) ->
    Url = application:get_env(analysis_service, lm_studio_url, "http://host.docker.internal:1234"),
    
    %% Check availability on startup
    Available = check_availability(Url),
    
    %% Schedule periodic availability checks
    erlang:send_after(30000, self(), check_availability),
    
    {ok, #state{url = Url, available = Available}}.

handle_call({analyze, Text, TopN}, _From, #state{url = Url, available = Available} = State) ->
    Result = case Available of
        true ->
            llm_analyze(Url, Text, TopN);
        false ->
            keyword_analyze(Text, TopN)
    end,
    {reply, Result, State};

handle_call({detect_biases, Text}, _From, #state{url = Url, available = Available} = State) ->
    Result = case Available of
        true ->
            llm_detect_biases(Url, Text);
        false ->
            keyword_detect_biases(Text)
    end,
    {reply, Result, State};

handle_call(is_available, _From, #state{available = Available} = State) ->
    {reply, Available, State};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(check_availability, #state{url = Url} = State) ->
    Available = check_availability(Url),
    erlang:send_after(30000, self(), check_availability),
    {noreply, State#state{available = Available}};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================

check_availability(Url) ->
    HealthUrl = Url ++ "/v1/models",
    case hackney:request(get, list_to_binary(HealthUrl), [], <<>>, [{timeout, 5000}]) of
        {ok, 200, _, _} -> true;
        _ -> false
    end.
