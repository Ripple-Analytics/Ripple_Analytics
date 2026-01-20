%%%-------------------------------------------------------------------
%%% @doc Tags Handler - Manage tags/labels for analyses
%%%-------------------------------------------------------------------
-module(tags_handler).
-behaviour(cowboy_handler).

-export([init/2]).

init(Req0, State) ->
    Method = cowboy_req:method(Req0),
    handle_request(Method, Req0, State).

handle_request(<<"GET">>, Req0, State) ->
    %% Get tags - either all unique tags or tags for a specific analysis
    QsVals = cowboy_req:parse_qs(Req0),
    AnalysisId = proplists:get_value(<<"analysis_id">>, QsVals, <<>>),
    
    Result = case AnalysisId of
        <<>> -> 
            %% Return all unique tags with counts
            AllTags = get_all_tags(),
            #{success => true, tags => AllTags};
        _ -> 
            %% Return tags for specific analysis
            Tags = get_tags_for_analysis(AnalysisId),
            #{success => true, tags => Tags, analysis_id => AnalysisId}
    end,
    
    Response = jsx:encode(Result),
    Req = cowboy_req:reply(200, #{<<"content-type">> => <<"application/json">>}, Response, Req0),
    {ok, Req, State};

handle_request(<<"POST">>, Req0, State) ->
    %% Add a tag to an analysis
    {ok, Body, Req1} = cowboy_req:read_body(Req0),
    Result = try
        Data = jsx:decode(Body, [return_maps]),
        AnalysisId = maps:get(<<"analysis_id">>, Data),
        TagName = maps:get(<<"tag">>, Data),
        
        add_tag(AnalysisId, TagName),
        {200, #{success => true, message => <<"Tag added">>}}
    catch
        _:_ ->
            {400, #{success => false, error => <<"Invalid request">>}}
    end,
    {Status, ResponseBody} = Result,
    ResponseJson = jsx:encode(ResponseBody),
    Req2 = cowboy_req:reply(Status, #{<<"content-type">> => <<"application/json">>}, ResponseJson, Req1),
    {ok, Req2, State};

handle_request(<<"DELETE">>, Req0, State) ->
    %% Remove a tag from an analysis
    QsVals = cowboy_req:parse_qs(Req0),
    AnalysisId = proplists:get_value(<<"analysis_id">>, QsVals, <<>>),
    TagName = proplists:get_value(<<"tag">>, QsVals, <<>>),
    
    case {AnalysisId, TagName} of
        {<<>>, _} ->
            Response = jsx:encode(#{success => false, error => <<"analysis_id required">>}),
            Req = cowboy_req:reply(400, #{<<"content-type">> => <<"application/json">>}, Response, Req0),
            {ok, Req, State};
        {_, <<>>} ->
            Response = jsx:encode(#{success => false, error => <<"tag required">>}),
            Req = cowboy_req:reply(400, #{<<"content-type">> => <<"application/json">>}, Response, Req0),
            {ok, Req, State};
        _ ->
            remove_tag(AnalysisId, TagName),
            Response = jsx:encode(#{success => true, message => <<"Tag removed">>}),
            Req = cowboy_req:reply(200, #{<<"content-type">> => <<"application/json">>}, Response, Req0),
            {ok, Req, State}
    end;

handle_request(_, Req0, State) ->
    Response = jsx:encode(#{error => <<"Method not allowed">>}),
    Req = cowboy_req:reply(405, #{<<"content-type">> => <<"application/json">>}, Response, Req0),
    {ok, Req, State}.

%% Internal functions

get_all_tags() ->
    case dets:open_file(tags_db, [{file, "/data/tags.dets"}, {type, bag}]) of
        {ok, Ref} ->
            %% Get all tags and count occurrences
            AllEntries = dets:foldl(fun({_AnalysisId, Tag}, Acc) -> [Tag | Acc] end, [], Ref),
            dets:close(Ref),
            %% Count occurrences
            TagCounts = lists:foldl(fun(Tag, Acc) ->
                Count = maps:get(Tag, Acc, 0),
                maps:put(Tag, Count + 1, Acc)
            end, #{}, AllEntries),
            %% Convert to list of maps
            [#{name => Tag, count => Count} || {Tag, Count} <- maps:to_list(TagCounts)];
        {error, _} ->
            []
    end.

get_tags_for_analysis(AnalysisId) ->
    case dets:open_file(tags_db, [{file, "/data/tags.dets"}, {type, bag}]) of
        {ok, Ref} ->
            Tags = case dets:lookup(Ref, AnalysisId) of
                [] -> [];
                Entries -> [Tag || {_, Tag} <- Entries]
            end,
            dets:close(Ref),
            Tags;
        {error, _} ->
            []
    end.

add_tag(AnalysisId, TagName) ->
    case dets:open_file(tags_db, [{file, "/data/tags.dets"}, {type, bag}]) of
        {ok, Ref} ->
            %% Check if tag already exists for this analysis
            ExistingTags = get_tags_for_analysis(AnalysisId),
            case lists:member(TagName, ExistingTags) of
                true -> ok;  %% Already exists
                false -> dets:insert(Ref, {AnalysisId, TagName})
            end,
            dets:close(Ref),
            ok;
        {error, _} ->
            error
    end.

remove_tag(AnalysisId, TagName) ->
    case dets:open_file(tags_db, [{file, "/data/tags.dets"}, {type, bag}]) of
        {ok, Ref} ->
            dets:delete_object(Ref, {AnalysisId, TagName}),
            dets:close(Ref),
            ok;
        {error, _} ->
            error
    end.
