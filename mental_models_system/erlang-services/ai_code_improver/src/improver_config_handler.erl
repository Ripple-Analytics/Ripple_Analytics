-module(improver_config_handler).
-behaviour(cowboy_handler).

-export([init/2]).

init(Req0, State) ->
    Method = cowboy_req:method(Req0),
    case Method of
        <<"GET">> ->
            handle_get(Req0, State);
        <<"POST">> ->
            handle_post(Req0, State);
        _ ->
            Req = cowboy_req:reply(405,
                #{<<"content-type">> => <<"application/json">>},
                jsx:encode(#{<<"error">> => <<"Method not allowed">>}),
                Req0),
            {ok, Req, State}
    end.

handle_get(Req0, State) ->
    Config = #{
        <<"lm_studio_url">> => list_to_binary(application:get_env(ai_code_improver, lm_studio_url, "http://host.docker.internal:1234")),
        <<"improvement_interval">> => application:get_env(ai_code_improver, improvement_interval, 300000),
        <<"max_changes_per_cycle">> => application:get_env(ai_code_improver, max_changes_per_cycle, 3),
        <<"auto_deploy">> => application:get_env(ai_code_improver, auto_deploy, true),
        <<"require_validation">> => application:get_env(ai_code_improver, require_validation, true),
        <<"auto_start">> => application:get_env(ai_code_improver, auto_start, false)
    },
    Response = jsx:encode(Config),
    Req = cowboy_req:reply(200,
        #{<<"content-type">> => <<"application/json">>},
        Response,
        Req0),
    {ok, Req, State}.

handle_post(Req0, State) ->
    {ok, Body, Req1} = cowboy_req:read_body(Req0),
    try
        Config = jsx:decode(Body, [return_maps]),
        
        case maps:get(<<"improvement_interval">>, Config, undefined) of
            undefined -> ok;
            Interval -> application:set_env(ai_code_improver, improvement_interval, Interval)
        end,
        
        case maps:get(<<"max_changes_per_cycle">>, Config, undefined) of
            undefined -> ok;
            MaxChanges -> application:set_env(ai_code_improver, max_changes_per_cycle, MaxChanges)
        end,
        
        case maps:get(<<"auto_deploy">>, Config, undefined) of
            undefined -> ok;
            AutoDeploy -> application:set_env(ai_code_improver, auto_deploy, AutoDeploy)
        end,
        
        case maps:get(<<"require_validation">>, Config, undefined) of
            undefined -> ok;
            RequireValidation -> application:set_env(ai_code_improver, require_validation, RequireValidation)
        end,
        
        Response = jsx:encode(#{<<"status">> => <<"updated">>, <<"message">> => <<"Configuration updated">>}),
        Req = cowboy_req:reply(200,
            #{<<"content-type">> => <<"application/json">>},
            Response,
            Req1),
        {ok, Req, State}
    catch
        _:_ ->
            Req = cowboy_req:reply(400,
                #{<<"content-type">> => <<"application/json">>},
                jsx:encode(#{<<"error">> => <<"Invalid JSON">>}),
                Req1),
            {ok, Req, State}
    end.
