%%%-------------------------------------------------------------------
%%% @doc Code Improver HTTP Handler
%%% 
%%% REST API for autonomous code improvement service.
%%% 
%%% Endpoints:
%%% GET /api/analysis/improver - Get status
%%% POST /api/analysis/improver/start - Start autonomous improvement
%%% POST /api/analysis/improver/stop - Stop autonomous improvement
%%% POST /api/analysis/improver/suggest - Suggest improvement for module
%%% POST /api/analysis/improver/apply - Apply an improvement
%%% GET /api/analysis/improver/history - Get improvement history
%%% GET /api/analysis/improver/config - Get configuration
%%% POST /api/analysis/improver/config - Update configuration
%%% @end
%%%-------------------------------------------------------------------
-module(code_improver_handler).

-export([init/2]).

init(Req0, State) ->
    Method = cowboy_req:method(Req0),
    Path = cowboy_req:path(Req0),
    Req = handle_request(Method, Path, Req0),
    {ok, Req, State}.

handle_request(<<"GET">>, Path, Req) ->
    case parse_action(Path) of
        status -> handle_get_status(Req);
        history -> handle_get_history(Req);
        config -> handle_get_config(Req);
        _ -> handle_get_status(Req)
    end;

handle_request(<<"POST">>, Path, Req0) ->
    case parse_action(Path) of
        start -> handle_start(Req0);
        stop -> handle_stop(Req0);
        suggest -> handle_suggest(Req0);
        apply_improvement -> handle_apply(Req0);
        config -> handle_set_config(Req0);
        _ -> json_response(400, #{<<"error">> => <<"Unknown action">>}, Req0)
    end;

handle_request(<<"OPTIONS">>, _, Req) ->
    cowboy_req:reply(200, cors_headers(), <<>>, Req);

handle_request(_, _, Req) ->
    json_response(405, #{<<"error">> => <<"Method not allowed">>}, Req).

parse_action(Path) ->
    case binary:match(Path, <<"/start">>) of
        nomatch ->
            case binary:match(Path, <<"/stop">>) of
                nomatch ->
                    case binary:match(Path, <<"/suggest">>) of
                        nomatch ->
                            case binary:match(Path, <<"/apply">>) of
                                nomatch ->
                                    case binary:match(Path, <<"/history">>) of
                                        nomatch ->
                                            case binary:match(Path, <<"/config">>) of
                                                nomatch -> status;
                                                _ -> config
                                            end;
                                        _ -> history
                                    end;
                                _ -> apply_improvement
                            end;
                        _ -> suggest
                    end;
                _ -> stop
            end;
        _ -> start
    end.

handle_get_status(Req) ->
    case code_improver_service:get_status() of
        {ok, Status} ->
            json_response(200, Status, Req);
        {error, Reason} ->
            json_response(500, #{<<"error">> => format_error(Reason)}, Req)
    end.

handle_get_history(Req) ->
    case code_improver_service:get_history() of
        {ok, History} ->
            json_response(200, #{<<"history">> => History}, Req);
        {error, Reason} ->
            json_response(500, #{<<"error">> => format_error(Reason)}, Req)
    end.

handle_get_config(Req) ->
    case code_improver_service:get_config() of
        {ok, Config} ->
            json_response(200, Config, Req);
        {error, Reason} ->
            json_response(500, #{<<"error">> => format_error(Reason)}, Req)
    end.

handle_start(Req) ->
    case code_improver_service:start_improving() of
        {ok, Result} ->
            json_response(200, Result, Req);
        {error, Reason} ->
            json_response(500, #{<<"error">> => format_error(Reason)}, Req)
    end.

handle_stop(Req) ->
    case code_improver_service:stop_improving() of
        {ok, Result} ->
            json_response(200, Result, Req);
        {error, Reason} ->
            json_response(500, #{<<"error">> => format_error(Reason)}, Req)
    end.

handle_suggest(Req0) ->
    {ok, Body, Req} = cowboy_req:read_body(Req0),
    case jsx:decode(Body, [return_maps]) of
        #{<<"module_path">> := ModulePath} ->
            case code_improver_service:suggest_improvement(ModulePath) of
                {ok, Improvement} ->
                    json_response(200, Improvement, Req);
                {error, Reason} ->
                    json_response(500, #{<<"error">> => format_error(Reason)}, Req)
            end;
        _ ->
            json_response(400, #{<<"error">> => <<"module_path required">>}, Req)
    end.

handle_apply(Req0) ->
    {ok, Body, Req} = cowboy_req:read_body(Req0),
    case jsx:decode(Body, [return_maps]) of
        Improvement when is_map(Improvement) ->
            case code_improver_service:apply_improvement(Improvement) of
                {ok, Result} ->
                    json_response(200, Result, Req);
                {error, Reason} ->
                    json_response(500, #{<<"error">> => format_error(Reason)}, Req)
            end;
        _ ->
            json_response(400, #{<<"error">> => <<"Invalid improvement data">>}, Req)
    end.

handle_set_config(Req0) ->
    {ok, Body, Req} = cowboy_req:read_body(Req0),
    case jsx:decode(Body, [return_maps]) of
        Config when is_map(Config) ->
            case code_improver_service:set_config(Config) of
                {ok, Result} ->
                    json_response(200, Result, Req);
                {error, Reason} ->
                    json_response(500, #{<<"error">> => format_error(Reason)}, Req)
            end;
        _ ->
            json_response(400, #{<<"error">> => <<"Invalid config">>}, Req)
    end.

json_response(Status, Body, Req) ->
    cowboy_req:reply(Status, cors_headers(), jsx:encode(Body), Req).

cors_headers() ->
    #{<<"content-type">> => <<"application/json">>,
      <<"access-control-allow-origin">> => <<"*">>,
      <<"access-control-allow-methods">> => <<"GET, POST, OPTIONS">>,
      <<"access-control-allow-headers">> => <<"Content-Type">>}.

format_error(Reason) when is_atom(Reason) -> atom_to_binary(Reason, utf8);
format_error(Reason) when is_binary(Reason) -> Reason;
format_error(Reason) -> list_to_binary(io_lib:format("~p", [Reason])).
