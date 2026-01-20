-module(deploy_switch_handler).
-behaviour(cowboy_handler).
-export([init/2]).

init(Req0, State) ->
    Result = deployment_manager:switch_environment(),
    Response = case Result of
        ok -> #{status => <<"switched">>};
        {error, Reason} -> #{status => <<"error">>, reason => Reason}
    end,
    Req = cowboy_req:reply(200,
        #{<<"content-type">> => <<"application/json">>},
        jiffy:encode(Response),
        Req0),
    {ok, Req, State}.
