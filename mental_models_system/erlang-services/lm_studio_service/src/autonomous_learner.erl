%%%-------------------------------------------------------------------
%%% @doc Autonomous Learner - Learning and knowledge persistence
%%% @end
%%%-------------------------------------------------------------------
-module(autonomous_learner).

-export([learn_from_cycle/2, load_knowledge_base/0, save_knowledge_base/1]).
-export([extract_success_patterns/1, extract_failure_patterns/1]).

-define(KNOWLEDGE_FILE, "/data/knowledge_base.json").

%%====================================================================
%% API
%%====================================================================

learn_from_cycle(CompletedImprovements, FailedImprovements) ->
    SuccessPatterns = extract_success_patterns(CompletedImprovements),
    FailurePatterns = extract_failure_patterns(FailedImprovements),
    SuccessPatterns ++ FailurePatterns.

extract_success_patterns(Improvements) ->
    [#{
        type => success,
        file_type => determine_file_type(maps:get(<<"file">>, I, <<>>)),
        description => maps:get(<<"description">>, I, <<>>)
    } || I <- Improvements].

extract_failure_patterns(Failures) ->
    [#{
        type => failure,
        reason => Reason
    } || {_Improvement, Reason} <- Failures].

load_knowledge_base() ->
    case file:read_file(?KNOWLEDGE_FILE) of
        {ok, Content} ->
            try jsx:decode(Content, [return_maps])
            catch _:_ -> #{}
            end;
        {error, _} ->
            #{}
    end.

save_knowledge_base(KnowledgeBase) ->
    filelib:ensure_dir(?KNOWLEDGE_FILE),
    Content = jsx:encode(KnowledgeBase, [pretty]),
    file:write_file(?KNOWLEDGE_FILE, Content).

%%====================================================================
%% Internal
%%====================================================================

determine_file_type(FilePath) ->
    case binary:match(FilePath, <<"_handler.erl">>) of
        nomatch ->
            case binary:match(FilePath, <<"_sup.erl">>) of
                nomatch -> other;
                _ -> supervisor
            end;
        _ -> handler
    end.
