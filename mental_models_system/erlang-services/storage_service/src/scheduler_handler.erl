%%%-------------------------------------------------------------------
%%% @doc Scheduler Handler - HTTP API for scheduled analysis tasks
%%% @end
%%%-------------------------------------------------------------------
-module(scheduler_handler).
-behaviour(cowboy_handler).

-export([init/2]).

init(Req0, State) ->
    Method = cowboy_req:method(Req0),
    Path = cowboy_req:path(Req0),
    
    {Status, Body} = case {Method, Path} of
        {<<"GET">>, <<"/scheduler">>} ->
            handle_list(Req0);
        {<<"GET">>, <<"/scheduler/due">>} ->
            handle_get_due(Req0);
        {<<"POST">>, <<"/scheduler">>} ->
            handle_add(Req0);
        {<<"PUT">>, _} ->
            handle_update(Req0);
        {<<"DELETE">>, _} ->
            handle_delete(Req0);
        _ ->
            {404, #{error => <<"Not found">>}}
    end,
    
    Req = cowboy_req:reply(Status,
        #{<<"content-type">> => <<"application/json">>,
          <<"access-control-allow-origin">> => <<"*">>},
        jsx:encode(Body),
        Req0),
    {ok, Req, State}.

handle_list(_Req) ->
    case scheduler_store:list_tasks() of
        {ok, Tasks} ->
            {200, #{tasks => Tasks}};
        {error, Reason} ->
            {500, #{error => list_to_binary(io_lib:format("~p", [Reason]))}}
    end.

handle_get_due(_Req) ->
    case scheduler_store:get_due_tasks() of
        {ok, Tasks} ->
            {200, #{due_tasks => Tasks}};
        {error, Reason} ->
            {500, #{error => list_to_binary(io_lib:format("~p", [Reason]))}}
    end.

handle_add(Req0) ->
    {ok, Body, _Req} = cowboy_req:read_body(Req0),
    case jsx:decode(Body, [return_maps]) of
        TaskData when is_map(TaskData) ->
            %% Convert interval to integer if it's a binary
            Interval = case maps:get(<<"interval">>, TaskData, 86400) of
                I when is_binary(I) -> binary_to_integer(I);
                I when is_integer(I) -> I;
                _ -> 86400
            end,
            
            ParsedData = #{
                name => maps:get(<<"name">>, TaskData, <<"Unnamed Task">>),
                type => binary_to_atom(maps:get(<<"type">>, TaskData, <<"url">>), utf8),
                target => maps:get(<<"target">>, TaskData, <<>>),
                interval => Interval
            },
            
            case scheduler_store:add_task(ParsedData) of
                {ok, Id} ->
                    {201, #{id => Id, message => <<"Task scheduled successfully">>}};
                {error, Reason} ->
                    {500, #{error => list_to_binary(io_lib:format("~p", [Reason]))}}
            end;
        _ ->
            {400, #{error => <<"Invalid JSON">>}}
    end.

handle_update(Req0) ->
    Path = cowboy_req:path(Req0),
    Id = extract_id(Path),
    
    {ok, Body, _Req} = cowboy_req:read_body(Req0),
    case jsx:decode(Body, [return_maps]) of
        Updates when is_map(Updates) ->
            %% Convert binary keys to atoms
            ParsedUpdates = parse_updates(Updates),
            case scheduler_store:update_task(Id, ParsedUpdates) of
                ok ->
                    {200, #{message => <<"Task updated">>}};
                {error, not_found} ->
                    {404, #{error => <<"Task not found">>}};
                {error, Reason} ->
                    {500, #{error => list_to_binary(io_lib:format("~p", [Reason]))}}
            end;
        _ ->
            {400, #{error => <<"Invalid JSON">>}}
    end.

handle_delete(Req0) ->
    Path = cowboy_req:path(Req0),
    Id = extract_id(Path),
    
    case scheduler_store:delete_task(Id) of
        ok ->
            {200, #{message => <<"Task deleted">>}};
        {error, Reason} ->
            {500, #{error => list_to_binary(io_lib:format("~p", [Reason]))}}
    end.

extract_id(Path) ->
    %% Extract ID from path like /scheduler/123456
    Parts = binary:split(Path, <<"/">>, [global, trim_all]),
    case Parts of
        [<<"scheduler">>, Id] -> Id;
        _ -> <<>>
    end.

parse_updates(Updates) ->
    maps:fold(
        fun(Key, Value, Acc) ->
            AtomKey = case Key of
                <<"name">> -> name;
                <<"type">> -> type;
                <<"target">> -> target;
                <<"interval">> -> interval;
                <<"enabled">> -> enabled;
                <<"last_run">> -> last_run;
                <<"next_run">> -> next_run;
                _ -> binary_to_atom(Key, utf8)
            end,
            ParsedValue = case AtomKey of
                type when is_binary(Value) -> binary_to_atom(Value, utf8);
                interval when is_binary(Value) -> binary_to_integer(Value);
                _ -> Value
            end,
            maps:put(AtomKey, ParsedValue, Acc)
        end,
        #{},
        Updates
    ).
