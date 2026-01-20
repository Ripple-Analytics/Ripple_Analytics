%%%-------------------------------------------------------------------
%%% @doc Backup Create Handler
%%% 
%%% Triggers manual backup creation.
%%% @end
%%%-------------------------------------------------------------------
-module(backup_create_handler).
-behaviour(cowboy_handler).

-export([init/2]).

init(Req0, State) ->
    Method = cowboy_req:method(Req0),
    
    case Method of
        <<"POST">> ->
            %% Trigger backup
            backup_scheduler:trigger_backup(),
            
            Response = #{
                <<"success">> => true,
                <<"message">> => <<"Backup triggered successfully">>,
                <<"timestamp">> => list_to_binary(iso8601_timestamp())
            },
            
            Req = cowboy_req:reply(200, 
                #{<<"content-type">> => <<"application/json">>,
                  <<"access-control-allow-origin">> => <<"*">>},
                jsx:encode(Response), Req0),
            {ok, Req, State};
        <<"OPTIONS">> ->
            Req = cowboy_req:reply(200, cors_headers(), <<>>, Req0),
            {ok, Req, State};
        _ ->
            Req = cowboy_req:reply(405, 
                #{<<"content-type">> => <<"application/json">>},
                jsx:encode(#{<<"error">> => <<"Method not allowed">>}), Req0),
            {ok, Req, State}
    end.

cors_headers() ->
    #{<<"content-type">> => <<"application/json">>,
      <<"access-control-allow-origin">> => <<"*">>,
      <<"access-control-allow-methods">> => <<"POST, OPTIONS">>,
      <<"access-control-allow-headers">> => <<"Content-Type">>}.

iso8601_timestamp() ->
    {{Y, M, D}, {H, Mi, S}} = calendar:universal_time(),
    io_lib:format("~4..0B-~2..0B-~2..0BT~2..0B:~2..0B:~2..0BZ", [Y, M, D, H, Mi, S]).
