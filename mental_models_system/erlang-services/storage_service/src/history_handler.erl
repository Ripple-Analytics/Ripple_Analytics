%%%-------------------------------------------------------------------
%%% @doc History Handler - Analysis history API endpoints
%%%-------------------------------------------------------------------
-module(history_handler).
-behaviour(cowboy_handler).

-export([init/2]).

init(Req0, State) ->
    Path = cowboy_req:path(Req0),
    case cowboy_req:method(Req0) of
        <<"GET">> ->
            handle_list(Req0, State);
        <<"POST">> ->
            {ok, Body, Req1} = cowboy_req:read_body(Req0),
            case Path of
                <<"/history/import">> ->
                    handle_import(Body, Req1, State);
                _ ->
                    handle_save(Body, Req1, State)
            end;
        <<"DELETE">> ->
            handle_clear(Req0, State);
        <<"OPTIONS">> ->
            Req = cowboy_req:reply(200, cors_headers(), <<>>, Req0),
            {ok, Req, State};
        _ ->
            Req = cowboy_req:reply(405, cors_headers(),
                jsx:encode(#{<<"error">> => <<"Method not allowed">>}), Req0),
            {ok, Req, State}
    end.

handle_list(Req0, State) ->
    QsVals = cowboy_req:parse_qs(Req0),
    Limit = case proplists:get_value(<<"limit">>, QsVals) of
        undefined -> 50;
        L -> binary_to_integer(L)
    end,
    Type = proplists:get_value(<<"type">>, QsVals, all),
    
    Opts = #{limit => Limit, type => Type},
    {ok, Analyses} = history_store:list_analyses(Opts),
    
    Response = jsx:encode(#{
        <<"success">> => true,
        <<"analyses">> => Analyses,
        <<"count">> => length(Analyses)
    }),
    Req = cowboy_req:reply(200, cors_headers(), Response, Req0),
    {ok, Req, State}.

handle_save(Body, Req0, State) ->
    try jsx:decode(Body, [return_maps]) of
        Analysis ->
            {ok, Saved} = history_store:save_analysis(Analysis),
            Response = jsx:encode(#{
                <<"success">> => true,
                <<"analysis">> => Saved
            }),
            Req = cowboy_req:reply(201, cors_headers(), Response, Req0),
            {ok, Req, State}
    catch
        _:_ ->
            Req = cowboy_req:reply(400, cors_headers(),
                jsx:encode(#{<<"error">> => <<"Invalid JSON">>}), Req0),
            {ok, Req, State}
    end.

handle_import(Body, Req0, State) ->
    try jsx:decode(Body, [return_maps]) of
        #{<<"analyses">> := Analyses} when is_list(Analyses) ->
            %% Import multiple analyses
            Imported = lists:foldl(fun(Analysis, Acc) ->
                case history_store:save_analysis(Analysis) of
                    {ok, _} -> Acc + 1;
                    _ -> Acc
                end
            end, 0, Analyses),
            Response = jsx:encode(#{
                <<"success">> => true,
                <<"imported">> => Imported,
                <<"total">> => length(Analyses)
            }),
            Req = cowboy_req:reply(201, cors_headers(), Response, Req0),
            {ok, Req, State};
        _ ->
            Req = cowboy_req:reply(400, cors_headers(),
                jsx:encode(#{<<"error">> => <<"Invalid format, expected {analyses: [...]}">>}), Req0),
            {ok, Req, State}
    catch
        _:_ ->
            Req = cowboy_req:reply(400, cors_headers(),
                jsx:encode(#{<<"error">> => <<"Invalid JSON">>}), Req0),
            {ok, Req, State}
    end.

handle_clear(Req0, State) ->
    case history_store:clear_all() of
        ok ->
            Response = jsx:encode(#{
                <<"success">> => true,
                <<"message">> => <<"All history cleared">>
            }),
            Req = cowboy_req:reply(200, cors_headers(), Response, Req0),
            {ok, Req, State};
        {error, Reason} ->
            Response = jsx:encode(#{
                <<"success">> => false,
                <<"error">> => list_to_binary(io_lib:format("~p", [Reason]))
            }),
            Req = cowboy_req:reply(500, cors_headers(), Response, Req0),
            {ok, Req, State}
    end.

cors_headers() ->
    #{<<"content-type">> => <<"application/json">>,
      <<"access-control-allow-origin">> => <<"*">>,
      <<"access-control-allow-methods">> => <<"GET, POST, DELETE, OPTIONS">>,
      <<"access-control-allow-headers">> => <<"Content-Type">>}.
