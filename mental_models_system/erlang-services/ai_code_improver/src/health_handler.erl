-module(health_handler).
-behaviour(cowboy_handler).

-export([init/2]).

init(Req0, State) ->
    Response = jsx:encode(#{
        <<"status">> => <<"healthy">>,
        <<"service">> => <<"ai_code_improver">>,
        <<"version">> => <<"1.0.0">>,
        <<"timestamp">> => list_to_binary(calendar:system_time_to_rfc3339(erlang:system_time(second)))
    }),
    Req = cowboy_req:reply(200,
        #{<<"content-type">> => <<"application/json">>},
        Response,
        Req0),
    {ok, Req, State}.
