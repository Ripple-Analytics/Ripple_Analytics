%%%-------------------------------------------------------------------
%%% @doc Notes Handler - Manage notes/annotations for analyses
%%%-------------------------------------------------------------------
-module(notes_handler).
-behaviour(cowboy_handler).

-export([init/2]).

init(Req0, State) ->
    Method = cowboy_req:method(Req0),
    handle_request(Method, Req0, State).

handle_request(<<"GET">>, Req0, State) ->
    %% Get notes for a specific analysis or all notes
    QsVals = cowboy_req:parse_qs(Req0),
    AnalysisId = proplists:get_value(<<"analysis_id">>, QsVals, <<>>),
    
    Notes = case AnalysisId of
        <<>> -> get_all_notes();
        _ -> get_notes_for_analysis(AnalysisId)
    end,
    
    Response = jsx:encode(#{success => true, notes => Notes}),
    Req = cowboy_req:reply(200, #{<<"content-type">> => <<"application/json">>}, Response, Req0),
    {ok, Req, State};

handle_request(<<"POST">>, Req0, State) ->
    %% Add a note
    {ok, Body, Req1} = cowboy_req:read_body(Req0),
    Result = try
        Data = jsx:decode(Body, [return_maps]),
        AnalysisId = maps:get(<<"analysis_id">>, Data),
        Content = maps:get(<<"content">>, Data),
        
        NoteId = generate_note_id(),
        Note = #{
            id => NoteId,
            analysis_id => AnalysisId,
            content => Content,
            created_at => erlang:system_time(second),
            updated_at => erlang:system_time(second)
        },
        
        add_note(Note),
        {200, #{success => true, note => Note}}
    catch
        _:_ ->
            {400, #{success => false, error => <<"Invalid request">>}}
    end,
    {Status, ResponseBody} = Result,
    ResponseJson = jsx:encode(ResponseBody),
    Req2 = cowboy_req:reply(Status, #{<<"content-type">> => <<"application/json">>}, ResponseJson, Req1),
    {ok, Req2, State};

handle_request(<<"PUT">>, Req0, State) ->
    %% Update a note
    {ok, Body, Req1} = cowboy_req:read_body(Req0),
    Result = try
        Data = jsx:decode(Body, [return_maps]),
        NoteId = maps:get(<<"id">>, Data),
        Content = maps:get(<<"content">>, Data),
        
        update_note(NoteId, Content),
        {200, #{success => true, message => <<"Note updated">>}}
    catch
        _:_ ->
            {400, #{success => false, error => <<"Invalid request">>}}
    end,
    {Status, ResponseBody} = Result,
    ResponseJson = jsx:encode(ResponseBody),
    Req2 = cowboy_req:reply(Status, #{<<"content-type">> => <<"application/json">>}, ResponseJson, Req1),
    {ok, Req2, State};

handle_request(<<"DELETE">>, Req0, State) ->
    %% Delete a note by id in query string
    QsVals = cowboy_req:parse_qs(Req0),
    NoteId = proplists:get_value(<<"id">>, QsVals, <<>>),
    
    case NoteId of
        <<>> ->
            Response = jsx:encode(#{success => false, error => <<"id required">>}),
            Req = cowboy_req:reply(400, #{<<"content-type">> => <<"application/json">>}, Response, Req0),
            {ok, Req, State};
        _ ->
            delete_note(NoteId),
            Response = jsx:encode(#{success => true, message => <<"Note deleted">>}),
            Req = cowboy_req:reply(200, #{<<"content-type">> => <<"application/json">>}, Response, Req0),
            {ok, Req, State}
    end;

handle_request(_, Req0, State) ->
    Response = jsx:encode(#{error => <<"Method not allowed">>}),
    Req = cowboy_req:reply(405, #{<<"content-type">> => <<"application/json">>}, Response, Req0),
    {ok, Req, State}.

%% Internal functions

generate_note_id() ->
    list_to_binary(integer_to_list(erlang:system_time(microsecond))).

get_all_notes() ->
    case dets:open_file(notes_db, [{file, "/data/notes.dets"}, {type, set}]) of
        {ok, Ref} ->
            Notes = dets:foldl(fun({_Key, Note}, Acc) -> [Note | Acc] end, [], Ref),
            dets:close(Ref),
            lists:sort(fun(A, B) -> 
                maps:get(created_at, A, 0) > maps:get(created_at, B, 0) 
            end, Notes);
        {error, _} ->
            []
    end.

get_notes_for_analysis(AnalysisId) ->
    AllNotes = get_all_notes(),
    [N || N <- AllNotes, maps:get(analysis_id, N, <<>>) =:= AnalysisId].

add_note(Note) ->
    case dets:open_file(notes_db, [{file, "/data/notes.dets"}, {type, set}]) of
        {ok, Ref} ->
            Id = maps:get(id, Note),
            dets:insert(Ref, {Id, Note}),
            dets:close(Ref),
            ok;
        {error, _} ->
            error
    end.

update_note(NoteId, Content) ->
    case dets:open_file(notes_db, [{file, "/data/notes.dets"}, {type, set}]) of
        {ok, Ref} ->
            case dets:lookup(Ref, NoteId) of
                [{_, Note}] ->
                    UpdatedNote = Note#{
                        content => Content,
                        updated_at => erlang:system_time(second)
                    },
                    dets:insert(Ref, {NoteId, UpdatedNote});
                [] ->
                    ok
            end,
            dets:close(Ref),
            ok;
        {error, _} ->
            error
    end.

delete_note(NoteId) ->
    case dets:open_file(notes_db, [{file, "/data/notes.dets"}, {type, set}]) of
        {ok, Ref} ->
            dets:delete(Ref, NoteId),
            dets:close(Ref),
            ok;
        {error, _} ->
            error
    end.
