%%%-------------------------------------------------------------------
%%% @doc Model Registry - Main Module
%%% 
%%% Stores and manages mental models with their categories,
%%% descriptions, and failure modes.
%%% @end
%%%-------------------------------------------------------------------
-module(model_registry).
-behaviour(gen_server).

-export([start_link/0, get_all_models/0, get_model/1, get_categories/0, 
         search_models/1, get_models_by_category/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-define(SERVER, ?MODULE).

-record(model, {
    id :: binary(),
    name :: binary(),
    category :: binary(),
    description :: binary(),
    key_insight :: binary(),
    application :: binary(),
    failure_modes :: [binary()],
    keywords :: [binary()]
}).

%%====================================================================
%% API
%%====================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

get_all_models() ->
    gen_server:call(?SERVER, get_all).

get_model(Id) ->
    gen_server:call(?SERVER, {get, Id}).

get_categories() ->
    gen_server:call(?SERVER, get_categories).

search_models(Query) ->
    gen_server:call(?SERVER, {search, Query}).

get_models_by_category(Category) ->
    gen_server:call(?SERVER, {by_category, Category}).

%%====================================================================
%% gen_server callbacks
%%====================================================================

init([]) ->
    ets:new(models, [named_table, public, {keypos, #model.id}]),
    load_all_models(),
    {ok, #{}}.

handle_call(get_all, _From, State) ->
    Models = ets:tab2list(models),
    ModelMaps = lists:map(fun(M) -> model_to_map(M) end, Models),
    {reply, ModelMaps, State};

handle_call({get, Id}, _From, State) ->
    case ets:lookup(models, Id) of
        [Model] -> {reply, {ok, Model}, State};
        [] -> {reply, {error, not_found}, State}
    end;

handle_call(get_categories, _From, State) ->
    Models = ets:tab2list(models),
    Categories = lists:usort([M#model.category || M <- Models]),
    {reply, Categories, State};

handle_call({search, Query}, _From, State) ->
    QueryLower = string:lowercase(binary_to_list(Query)),
    Models = ets:tab2list(models),
    Matches = lists:filter(fun(M) ->
        NameLower = string:lowercase(binary_to_list(M#model.name)),
        DescLower = string:lowercase(binary_to_list(M#model.description)),
        string:find(NameLower, QueryLower) =/= nomatch orelse
        string:find(DescLower, QueryLower) =/= nomatch
    end, Models),
    {reply, Matches, State};

handle_call({by_category, Category}, _From, State) ->
    Models = ets:tab2list(models),
    Matches = [M || M <- Models, M#model.category =:= Category],
    {reply, Matches, State};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================

model_to_map(#model{id = Id, name = Name, category = Category, 
                    description = Description, key_insight = KeyInsight,
                    application = Application, failure_modes = FailureModes,
                    keywords = Keywords}) ->
    #{
        <<"id">> => Id,
        <<"name">> => Name,
        <<"category">> => Category,
        <<"description">> => Description,
        <<"key_insight">> => KeyInsight,
        <<"application">> => Application,
        <<"failure_modes">> => FailureModes,
        <<"keywords">> => Keywords
    }.

load_all_models() ->
    DataModules = [
        model_data_psychology_biases_and_tendencies, model_data_economics_and_finance, model_data_systems_thinking, model_data_physics_and_engineering, model_data_biology_and_evolution, model_data_mathematics_and_statistics, model_data_organizational_and_moats, model_data_physics_and_engineering, model_data_biology_and_evolution, model_data_mathematics_and_statistics, model_data_economics_and_markets, model_data_systems_thinking, model_data_military_and_strategy, model_data_psychology_advanced, model_data_communication_and_influence, model_data_investing_and_finance, model_data_technology_and_innovation, model_data_leadership_and_management, model_data_learning_and_knowledge, model_data_time_and_productivity, model_data_negotiation_and_persuasion, model_data_risk_and_uncertainty_part1, model_data_risk_and_uncertainty_part2
    ],
    lists:foreach(fun(Mod) ->
        Models = Mod:get_models(),
        lists:foreach(fun(Model) ->
            ets:insert(models, Model)
        end, Models)
    end, DataModules).
