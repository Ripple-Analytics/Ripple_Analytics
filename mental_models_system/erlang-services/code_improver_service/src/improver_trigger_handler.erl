-module(improver_trigger_handler).
-behaviour(cowboy_handler).

-export([init/2]).

init(Req0, State) ->
    Method = cowboy_req:method(Req0),
    handle_request(Method, Req0, State).

handle_request(<<"POST">>, Req0, State) ->
    io:format("[TRIGGER] Manual improvement cycle triggered~n"),
    
    ok = improver_worker:trigger_improvement_cycle(),
    
    Response = #{
        <<"status">> => <<"triggered">>,
        <<"message">> => <<"Improvement cycle started">>,
        <<"timestamp">> => erlang:system_time(second)
    },
    
    Body = jsx:encode(Response),
    Req = cowboy_req:reply(200,
        #{<<"content-type">> => <<"application/json">>},
        Body,
        Req0),
    {ok, Req, State};

handle_request(_, Req0, State) ->
    Req = cowboy_req:reply(405,
        #{<<"content-type">> => <<"application/json">>},
        jsx:encode(#{<<"error">> => <<"Method not allowed. Use POST to trigger.">>}),
        Req0),
    {ok, Req, State}.
