-module(services_health_handler).
-behaviour(cowboy_handler).
-export([init/2]).

init(Req0, State) ->
    Health = chaos_controller:get_service_health(),
    Req = cowboy_req:reply(200, cors_headers(),
        jsx:encode(#{<<"services">> => Health}), Req0),
    {ok, Req, State}.

cors_headers() ->
    #{<<"content-type">> => <<"application/json">>,
      <<"access-control-allow-origin">> => <<"*">>}.
