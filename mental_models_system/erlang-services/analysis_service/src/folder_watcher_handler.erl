%%%-------------------------------------------------------------------
%%% @doc Folder Watcher HTTP Handler
%%% 
%%% REST API for controlling the folder watcher service.
%%% 
%%% Endpoints:
%%% GET  /api/analysis/watcher - Get watcher status
%%% POST /api/analysis/watcher - Control watcher (start/stop/config)
%%% @end
%%%-------------------------------------------------------------------
-module(folder_watcher_handler).

-export([init/2]).

init(Req0, State) ->
    Method = cowboy_req:method(Req0),
    Req = handle_request(Method, Req0),
    {ok, Req, State}.

handle_request(<<"GET">>, Req) ->
    case folder_watcher:get_status() of
        {ok, Status} ->
            Results = case folder_watcher:get_results(10) of
                {ok, R} -> R;
                _ -> []
            end,
            Response = Status#{<<"recent_results">> => Results},
            json_response(200, Response, Req);
        {error, Reason} ->
            json_response(500, #{<<"error">> => format_error(Reason)}, Req)
    end;

handle_request(<<"POST">>, Req0) ->
    {ok, Body, Req} = cowboy_req:read_body(Req0),
    case jsx:decode(Body, [return_maps]) of
        #{<<"action">> := Action} = Params ->
            handle_action(Action, Params, Req);
        _ ->
            json_response(400, #{
                <<"error">> => <<"Missing action parameter">>,
                <<"valid_actions">> => [<<"start">>, <<"stop">>, <<"config">>, <<"clear">>, <<"results">>]
            }, Req)
    end;

handle_request(_, Req) ->
    json_response(405, #{<<"error">> => <<"Method not allowed">>}, Req).

handle_action(<<"start">>, Params, Req) ->
    Config = build_config(Params),
    case folder_watcher:start_watching(Config) of
        {ok, Result} ->
            json_response(200, #{<<"success">> => true, <<"result">> => Result}, Req);
        {error, Reason} ->
            json_response(500, #{<<"success">> => false, <<"error">> => format_error(Reason)}, Req)
    end;

handle_action(<<"stop">>, _Params, Req) ->
    case folder_watcher:stop_watching() of
        {ok, Result} ->
            json_response(200, #{<<"success">> => true, <<"result">> => Result}, Req);
        {error, Reason} ->
            json_response(500, #{<<"success">> => false, <<"error">> => format_error(Reason)}, Req)
    end;

handle_action(<<"config">>, Params, Req) ->
    Config = build_config(Params),
    case folder_watcher:set_config(Config) of
        {ok, Result} ->
            json_response(200, #{<<"success">> => true, <<"config">> => Result}, Req);
        {error, Reason} ->
            json_response(500, #{<<"success">> => false, <<"error">> => format_error(Reason)}, Req)
    end;

handle_action(<<"clear">>, _Params, Req) ->
    case folder_watcher:clear_results() of
        {ok, Result} ->
            json_response(200, #{<<"success">> => true, <<"result">> => Result}, Req);
        {error, Reason} ->
            json_response(500, #{<<"success">> => false, <<"error">> => format_error(Reason)}, Req)
    end;

handle_action(<<"results">>, Params, Req) ->
    Limit = maps:get(<<"limit">>, Params, 50),
    case folder_watcher:get_results(Limit) of
        {ok, Results} ->
            json_response(200, #{<<"success">> => true, <<"results">> => Results, <<"count">> => length(Results)}, Req);
        {error, Reason} ->
            json_response(500, #{<<"success">> => false, <<"error">> => format_error(Reason)}, Req)
    end;

handle_action(Action, _Params, Req) ->
    json_response(400, #{
        <<"error">> => <<"Unknown action">>,
        <<"action">> => Action,
        <<"valid_actions">> => [<<"start">>, <<"stop">>, <<"config">>, <<"clear">>, <<"results">>]
    }, Req).

build_config(Params) ->
    Config0 = #{},
    Config1 = case maps:get(<<"folder_path">>, Params, undefined) of
        undefined -> Config0;
        Path -> Config0#{folder_path => Path}
    end,
    case maps:get(<<"interval">>, Params, undefined) of
        undefined -> Config1;
        Interval when is_integer(Interval) -> Config1#{interval => Interval};
        _ -> Config1
    end.

json_response(Status, Body, Req) ->
    cowboy_req:reply(Status, #{
        <<"content-type">> => <<"application/json">>,
        <<"access-control-allow-origin">> => <<"*">>
    }, jsx:encode(Body), Req).

format_error(Reason) when is_atom(Reason) ->
    atom_to_binary(Reason, utf8);
format_error(Reason) when is_binary(Reason) ->
    Reason;
format_error(Reason) ->
    list_to_binary(io_lib:format("~p", [Reason])).
