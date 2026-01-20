%%%-------------------------------------------------------------------
%%% @doc Favorites Handler - Manage bookmarked mental models
%%%-------------------------------------------------------------------
-module(favorites_handler).
-behaviour(cowboy_handler).

-export([init/2]).

init(Req0, State) ->
    Method = cowboy_req:method(Req0),
    handle_request(Method, Req0, State).

handle_request(<<"GET">>, Req0, State) ->
    %% Get all favorites
    Favorites = get_favorites(),
    Response = jsx:encode(#{success => true, favorites => Favorites}),
    Req = cowboy_req:reply(200, #{<<"content-type">> => <<"application/json">>}, Response, Req0),
    {ok, Req, State};

handle_request(<<"POST">>, Req0, State) ->
    %% Add a favorite
    {ok, Body, Req1} = cowboy_req:read_body(Req0),
    try
        Data = jsx:decode(Body, [return_maps]),
        ModelId = maps:get(<<"model_id">>, Data),
        ModelName = maps:get(<<"model_name">>, Data, ModelId),
        Category = maps:get(<<"category">>, Data, <<"unknown">>),
        
        Favorite = #{
            id => ModelId,
            name => ModelName,
            category => Category,
            added_at => erlang:system_time(second)
        },
        
        add_favorite(Favorite),
        
        SuccessResponse = jsx:encode(#{success => true, message => <<"Favorite added">>}),
        SuccessReq = cowboy_req:reply(200, #{<<"content-type">> => <<"application/json">>}, SuccessResponse, Req1),
        {ok, SuccessReq, State}
    catch
        _:_ ->
            ErrorResponse = jsx:encode(#{success => false, error => <<"Invalid request">>}),
            ErrorReq = cowboy_req:reply(400, #{<<"content-type">> => <<"application/json">>}, ErrorResponse, Req1),
            {ok, ErrorReq, State}
    end;

handle_request(<<"DELETE">>, Req0, State) ->
    %% Remove a favorite by model_id in query string
    QsVals = cowboy_req:parse_qs(Req0),
    ModelId = proplists:get_value(<<"model_id">>, QsVals, <<>>),
    
    case ModelId of
        <<>> ->
            Response = jsx:encode(#{success => false, error => <<"model_id required">>}),
            Req = cowboy_req:reply(400, #{<<"content-type">> => <<"application/json">>}, Response, Req0),
            {ok, Req, State};
        _ ->
            remove_favorite(ModelId),
            Response = jsx:encode(#{success => true, message => <<"Favorite removed">>}),
            Req = cowboy_req:reply(200, #{<<"content-type">> => <<"application/json">>}, Response, Req0),
            {ok, Req, State}
    end;

handle_request(_, Req0, State) ->
    Response = jsx:encode(#{error => <<"Method not allowed">>}),
    Req = cowboy_req:reply(405, #{<<"content-type">> => <<"application/json">>}, Response, Req0),
    {ok, Req, State}.

%% Internal functions

get_favorites() ->
    case dets:open_file(favorites_db, [{file, "/data/favorites.dets"}, {type, set}]) of
        {ok, Ref} ->
            Favorites = dets:foldl(fun({_Key, Fav}, Acc) -> [Fav | Acc] end, [], Ref),
            dets:close(Ref),
            Favorites;
        {error, _} ->
            []
    end.

add_favorite(Favorite) ->
    case dets:open_file(favorites_db, [{file, "/data/favorites.dets"}, {type, set}]) of
        {ok, Ref} ->
            Id = maps:get(id, Favorite),
            dets:insert(Ref, {Id, Favorite}),
            dets:close(Ref),
            ok;
        {error, _} ->
            error
    end.

remove_favorite(ModelId) ->
    case dets:open_file(favorites_db, [{file, "/data/favorites.dets"}, {type, set}]) of
        {ok, Ref} ->
            dets:delete(Ref, ModelId),
            dets:close(Ref),
            ok;
        {error, _} ->
            error
    end.
