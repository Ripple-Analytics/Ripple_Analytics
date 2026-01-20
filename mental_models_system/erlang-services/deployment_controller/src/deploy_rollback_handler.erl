-module(deploy_rollback_handler).
-behaviour(cowboy_handler).
-export([init/2]).

init(Req0, State) ->
    Result = deployment_manager:rollback(),
    Response = case Result of
        ok -> #{status => <<"rolled_back">>};
        {error, Reason} -> #{status => <<"error">>, reason => Reason}
    end,
    Req = cowboy_req:reply(200,
        #{<<"content-type">> => <<"application/json">>},
        jiffy:encode(Response),
        Req0),
    {ok, Req, State}.
