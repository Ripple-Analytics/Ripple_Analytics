-module(health_handler).
-behaviour(cowboy_handler).

-export([init/2]).

init(Req0, State) ->
    Status = improver_worker:get_status(),
    
    Response = #{
        <<"status">> => <<"healthy">>,
        <<"service">> => <<"code_improver_service">>,
        <<"version">> => <<"1.0.0">>,
        <<"worker_status">> => maps:get(status, Status, idle),
        <<"lm_studio_connected">> => check_lm_studio(),
        <<"timestamp">> => erlang:system_time(second)
    },
    
    Body = jsx:encode(Response),
    Req = cowboy_req:reply(200,
        #{<<"content-type">> => <<"application/json">>},
        Body,
        Req0),
    {ok, Req, State}.

check_lm_studio() ->
    Status = improver_worker:get_status(),
    LmStudioUrl = binary_to_list(maps:get(lm_studio_url, Status, <<"http://localhost:1234">>)),
    case lm_studio_client:health_check(LmStudioUrl) of
        {ok, healthy} -> true;
        _ -> false
    end.
