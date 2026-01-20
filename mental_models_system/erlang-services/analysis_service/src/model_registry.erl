%%%-------------------------------------------------------------------
%%% @doc Model Registry - Mental models storage and retrieval
%%% Models are loaded from separate data modules for maintainability
%%% @end
%%%-------------------------------------------------------------------
-module(model_registry).
-behaviour(gen_server).

-export([start_link/0, get_all_models/0, get_model/1, get_categories/0]).
-export([search_models/1, get_models_by_category/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-define(SERVER, ?MODULE).

-record(model, {
    id, name, category, description, key_insight, application, 
    failure_modes, keywords
}).

%% API
start_link() -> gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).
get_all_models() -> gen_server:call(?SERVER, get_all).
get_model(Id) -> gen_server:call(?SERVER, {get, Id}).
get_categories() -> gen_server:call(?SERVER, get_categories).
search_models(Query) -> gen_server:call(?SERVER, {search, Query}).
get_models_by_category(Cat) -> gen_server:call(?SERVER, {by_category, Cat}).

%% gen_server callbacks
init([]) ->
    ets:new(models, [named_table, public, {keypos, #model.id}]),
    load_all_models(),
    {ok, #{}}.

handle_call(get_all, _From, State) ->
    Models = [model_to_map(M) || M <- ets:tab2list(models)],
    {reply, Models, State};

handle_call({get, Id}, _From, State) ->
    case ets:lookup(models, Id) of
        [M] -> {reply, {ok, model_to_map(M)}, State};
        [] -> {reply, {error, not_found}, State}
    end;

handle_call(get_categories, _From, State) ->
    Cats = lists:usort([M#model.category || M <- ets:tab2list(models)]),
    {reply, Cats, State};

handle_call({search, Query}, _From, State) ->
    Q = string:lowercase(binary_to_list(Query)),
    Matches = [M || M <- ets:tab2list(models), matches(M, Q)],
    {reply, [model_to_map(M) || M <- Matches], State};

handle_call({by_category, Cat}, _From, State) ->
    Matches = [M || M <- ets:tab2list(models), M#model.category =:= Cat],
    {reply, [model_to_map(M) || M <- Matches], State};

handle_call(_, _From, State) -> {reply, ok, State}.
handle_cast(_, State) -> {noreply, State}.
handle_info(_, State) -> {noreply, State}.
terminate(_, _) -> ok.

%% Internal
matches(M, Q) ->
    N = string:lowercase(binary_to_list(M#model.name)),
    D = string:lowercase(binary_to_list(M#model.description)),
    string:find(N, Q) =/= nomatch orelse string:find(D, Q) =/= nomatch.

model_to_map(#model{id=Id, name=N, category=C, description=D, 
                    key_insight=K, application=A, failure_modes=F, keywords=W}) ->
    #{<<"id">>=>Id, <<"name">>=>N, <<"category">>=>C, <<"description">>=>D,
      <<"key_insight">>=>K, <<"application">>=>A, <<"failure_modes">>=>F, <<"keywords">>=>W}.

load_all_models() ->
    lists:foreach(fun(Mod) -> 
        Models = Mod:get_models(),
        [ets:insert(models, to_record(M)) || M <- Models]
    end, [models_thinking, models_psychology, models_systems, models_economics,
          models_science, models_strategy, models_communication]).

to_record(#{id:=Id, name:=N, category:=C, description:=D, key_insight:=K,
            application:=A, failure_modes:=F, keywords:=W}) ->
    #model{id=Id, name=N, category=C, description=D, key_insight=K,
           application=A, failure_modes=F, keywords=W}.
