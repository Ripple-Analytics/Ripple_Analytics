%%%-------------------------------------------------------------------
%%% @doc UI State - Manages application state and caching
%%%-------------------------------------------------------------------
-module(ui_state).
-behaviour(gen_server).

-export([start_link/0, get_models/0, get_categories/0, cache_models/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

get_models() ->
    gen_server:call(?MODULE, get_models).

get_categories() ->
    gen_server:call(?MODULE, get_categories).

cache_models(Models) ->
    gen_server:cast(?MODULE, {cache_models, Models}).

init([]) ->
    {ok, #{models => [], categories => [], last_fetch => 0}}.

handle_call(get_models, _From, #{models := Models} = State) ->
    {reply, Models, State};

handle_call(get_categories, _From, #{categories := Categories} = State) ->
    {reply, Categories, State};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({cache_models, Models}, State) ->
    Categories = lists:usort([maps:get(<<"category">>, M, <<"Unknown">>) || M <- Models]),
    {noreply, State#{models := Models, categories := Categories, last_fetch := erlang:system_time(second)}};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.
