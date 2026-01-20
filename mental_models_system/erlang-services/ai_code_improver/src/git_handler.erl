-module(git_handler).
-behaviour(cowboy_handler).

-export([init/2]).

init(Req0, State) ->
    Method = cowboy_req:method(Req0),
    Path = cowboy_req:path(Req0),
    case {Method, Path} of
        {<<"GET">>, <<"/api/git/status">>} ->
            handle_status(Req0, State);
        {<<"GET">>, <<"/api/git/branch">>} ->
            handle_branch(Req0, State);
        {<<"GET">>, <<"/api/git/commits">>} ->
            handle_commits(Req0, State);
        {<<"POST">>, <<"/api/git/commit">>} ->
            handle_commit(Req0, State);
        {<<"POST">>, <<"/api/git/push">>} ->
            handle_push(Req0, State);
        {<<"POST">>, <<"/api/git/branch">>} ->
            handle_create_branch(Req0, State);
        _ ->
            Req = cowboy_req:reply(404,
                #{<<"content-type">> => <<"application/json">>},
                jsx:encode(#{<<"error">> => <<"Not found">>}),
                Req0),
            {ok, Req, State}
    end.

handle_status(Req0, State) ->
    case git_integration:get_status() of
        {ok, Status} ->
            Req = cowboy_req:reply(200,
                #{<<"content-type">> => <<"application/json">>},
                jsx:encode(Status),
                Req0);
        {error, Reason} ->
            Req = cowboy_req:reply(500,
                #{<<"content-type">> => <<"application/json">>},
                jsx:encode(#{<<"error">> => format_error(Reason)}),
                Req0)
    end,
    {ok, Req, State}.

handle_branch(Req0, State) ->
    case git_integration:get_current_branch() of
        {ok, Branch} ->
            Req = cowboy_req:reply(200,
                #{<<"content-type">> => <<"application/json">>},
                jsx:encode(#{<<"branch">> => Branch}),
                Req0);
        {error, Reason} ->
            Req = cowboy_req:reply(500,
                #{<<"content-type">> => <<"application/json">>},
                jsx:encode(#{<<"error">> => format_error(Reason)}),
                Req0)
    end,
    {ok, Req, State}.

handle_commits(Req0, State) ->
    QsVals = cowboy_req:parse_qs(Req0),
    Count = case lists:keyfind(<<"count">>, 1, QsVals) of
        {_, CountBin} -> binary_to_integer(CountBin);
        false -> 10
    end,
    case git_integration:get_recent_commits(Count) of
        {ok, Commits} ->
            Req = cowboy_req:reply(200,
                #{<<"content-type">> => <<"application/json">>},
                jsx:encode(#{<<"commits">> => Commits}),
                Req0);
        {error, Reason} ->
            Req = cowboy_req:reply(500,
                #{<<"content-type">> => <<"application/json">>},
                jsx:encode(#{<<"error">> => format_error(Reason)}),
                Req0)
    end,
    {ok, Req, State}.

handle_commit(Req0, State) ->
    {ok, Body, Req1} = cowboy_req:read_body(Req0),
    try
        Request = jsx:decode(Body, [return_maps]),
        FilePath = binary_to_list(maps:get(<<"file">>, Request)),
        Description = binary_to_list(maps:get(<<"description">>, Request)),
        case git_integration:commit_improvement(FilePath, Description) of
            {ok, CommitHash} ->
                Req = cowboy_req:reply(200,
                    #{<<"content-type">> => <<"application/json">>},
                    jsx:encode(#{<<"status">> => <<"committed">>, <<"hash">> => CommitHash}),
                    Req1);
            {error, Reason} ->
                Req = cowboy_req:reply(500,
                    #{<<"content-type">> => <<"application/json">>},
                    jsx:encode(#{<<"error">> => format_error(Reason)}),
                    Req1)
        end,
        {ok, Req, State}
    catch
        _:_ ->
            Req = cowboy_req:reply(400,
                #{<<"content-type">> => <<"application/json">>},
                jsx:encode(#{<<"error">> => <<"Invalid JSON">>}),
                Req1),
            {ok, Req, State}
    end.

handle_push(Req0, State) ->
    case git_integration:push_changes() of
        {ok, pushed} ->
            Req = cowboy_req:reply(200,
                #{<<"content-type">> => <<"application/json">>},
                jsx:encode(#{<<"status">> => <<"pushed">>}),
                Req0);
        {error, Reason} ->
            Req = cowboy_req:reply(500,
                #{<<"content-type">> => <<"application/json">>},
                jsx:encode(#{<<"error">> => format_error(Reason)}),
                Req0)
    end,
    {ok, Req, State}.

handle_create_branch(Req0, State) ->
    {ok, Body, Req1} = cowboy_req:read_body(Req0),
    try
        Request = jsx:decode(Body, [return_maps]),
        BranchName = binary_to_list(maps:get(<<"name">>, Request)),
        case git_integration:create_branch(BranchName) of
            {ok, _} ->
                Req = cowboy_req:reply(200,
                    #{<<"content-type">> => <<"application/json">>},
                    jsx:encode(#{<<"status">> => <<"created">>, <<"branch">> => list_to_binary(BranchName)}),
                    Req1);
            {error, Reason} ->
                Req = cowboy_req:reply(500,
                    #{<<"content-type">> => <<"application/json">>},
                    jsx:encode(#{<<"error">> => format_error(Reason)}),
                    Req1)
        end,
        {ok, Req, State}
    catch
        _:_ ->
            Req = cowboy_req:reply(400,
                #{<<"content-type">> => <<"application/json">>},
                jsx:encode(#{<<"error">> => <<"Invalid JSON">>}),
                Req1),
            {ok, Req, State}
    end.

format_error(Reason) when is_binary(Reason) -> Reason;
format_error(Reason) when is_atom(Reason) -> atom_to_binary(Reason, utf8);
format_error(Reason) -> list_to_binary(io_lib:format("~p", [Reason])).
