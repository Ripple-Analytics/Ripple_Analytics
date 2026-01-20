%%%-------------------------------------------------------------------
%%% @doc Stats Collector - Tracks harvesting statistics
%%%-------------------------------------------------------------------
-module(stats_collector).
-behaviour(gen_server).

-export([start_link/0, record_scrape/1, record_process/1, record_batch_scrape/2, get_stats/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

record_scrape(Result) ->
    gen_server:cast(?MODULE, {scrape, Result}).

record_process(Result) ->
    gen_server:cast(?MODULE, {process, Result}).

record_batch_scrape(Total, Successful) ->
    gen_server:cast(?MODULE, {batch_scrape, Total, Successful}).

get_stats() ->
    gen_server:call(?MODULE, get_stats).

init([]) ->
    {ok, #{
        urls_scraped => 0,
        files_processed => 0,
        bytes_downloaded => 0,
        batch_scrapes => 0,
        batch_urls_total => 0,
        batch_urls_successful => 0,
        errors => 0,
        start_time => erlang:system_time(second)
    }}.

handle_call(get_stats, _From, State) ->
    Uptime = erlang:system_time(second) - maps:get(start_time, State),
    Stats = maps:put(uptime_seconds, Uptime, State),
    {reply, Stats, State};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({scrape, {ok, #{<<"size">> := Size}}}, State) ->
    NewState = State#{
        urls_scraped := maps:get(urls_scraped, State) + 1,
        bytes_downloaded := maps:get(bytes_downloaded, State) + Size
    },
    {noreply, NewState};

handle_cast({scrape, {error, _}}, State) ->
    {noreply, State#{errors := maps:get(errors, State) + 1}};

handle_cast({process, {ok, _}}, State) ->
    {noreply, State#{files_processed := maps:get(files_processed, State) + 1}};

handle_cast({process, {error, _}}, State) ->
    {noreply, State#{errors := maps:get(errors, State) + 1}};

handle_cast({batch_scrape, Total, Successful}, State) ->
    NewState = State#{
        batch_scrapes := maps:get(batch_scrapes, State) + 1,
        batch_urls_total := maps:get(batch_urls_total, State) + Total,
        batch_urls_successful := maps:get(batch_urls_successful, State) + Successful,
        errors := maps:get(errors, State) + (Total - Successful)
    },
    {noreply, NewState};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.
