%%%-------------------------------------------------------------------
%%% @doc Disable Handler for Chaos Monkey
%%% @end
%%%-------------------------------------------------------------------
-module(cm_disable_handler).
-behaviour(cowboy_handler).

-export([init/2]).

init(Req0, State) ->
    Method = cowboy_req:method(Req0),
    
    case Method of
        <<"POST">> ->
            chaos_engine:disable(),
            
            Response = #{
                <<"success">> => true,
                <<"message">> => <<"Chaos Monkey disabled">>,
                <<"info">> => <<"No attacks will be executed">>
            },
            
            Req = cowboy_req:reply(200, cors_headers(), jsx:encode(Response), Req0),
            {ok, Req, State};
        <<"OPTIONS">> ->
            Req = cowboy_req:reply(200, cors_headers(), <<>>, Req0),
            {ok, Req, State};
        _ ->
            Req = cowboy_req:reply(405, cors_headers(),
                jsx:encode(#{<<"error">> => <<"Method not allowed">>}), Req0),
            {ok, Req, State}
    end.

cors_headers() ->
    #{<<"content-type">> => <<"application/json">>,
      <<"access-control-allow-origin">> => <<"*">>,
      <<"access-control-allow-methods">> => <<"POST, OPTIONS">>,
      <<"access-control-allow-headers">> => <<"Content-Type">>}.
