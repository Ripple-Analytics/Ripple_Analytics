%%%-------------------------------------------------------------------
%%% @doc Updater Check Handler
%%% POST /api/updater/check - Triggers a manual update check
%%% @end
%%%-------------------------------------------------------------------
-module(updater_check_handler).
-behaviour(cowboy_handler).

-export([init/2]).

init(Req0, State) ->
    Method = cowboy_req:method(Req0),
    {StatusCode, Response} = handle_request(Method),
    Req = cowboy_req:reply(StatusCode,
        #{<<"content-type">> => <<"application/json">>,
          <<"access-control-allow-origin">> => <<"*">>,
          <<"access-control-allow-methods">> => <<"POST, OPTIONS">>},
        Response,
        Req0),
    {ok, Req, State}.

handle_request(<<"POST">>) ->
    case updater_worker:check_updates() of
        {ok, Result} ->
            {200, jsx:encode(Result)};
        {error, Reason} ->
            {500, jsx:encode(#{error => Reason})}
    end;
handle_request(<<"OPTIONS">>) ->
    {200, <<"{}">>};
handle_request(_) ->
    {405, jsx:encode(#{error => <<"Method not allowed">>})}.
