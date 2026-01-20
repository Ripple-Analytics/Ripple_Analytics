%%%-------------------------------------------------------------------
%%% @doc Health Handler for Google Drive Backup Service
%%% @end
%%%-------------------------------------------------------------------
-module(health_handler).
-behaviour(cowboy_handler).

-export([init/2]).

init(Req0, State) ->
    %% Check if all components are running
    GdriveStatus = try gdrive_client:get_status() of
        {ok, _} -> healthy;
        _ -> unhealthy
    catch
        _:_ -> unhealthy
    end,
    
    SchedulerStatus = try backup_scheduler:get_schedule() of
        {ok, _} -> healthy;
        _ -> unhealthy
    catch
        _:_ -> unhealthy
    end,
    
    OverallStatus = case {GdriveStatus, SchedulerStatus} of
        {healthy, healthy} -> <<"healthy">>;
        _ -> <<"degraded">>
    end,
    
    Response = #{
        <<"status">> => OverallStatus,
        <<"service">> => <<"gdrive-backup">>,
        <<"version">> => <<"1.0.0">>,
        <<"components">> => #{
            <<"gdrive_client">> => atom_to_binary(GdriveStatus),
            <<"backup_scheduler">> => atom_to_binary(SchedulerStatus)
        },
        <<"timestamp">> => list_to_binary(iso8601_timestamp())
    },
    
    StatusCode = case OverallStatus of
        <<"healthy">> -> 200;
        _ -> 503
    end,
    
    Req = cowboy_req:reply(StatusCode, 
        #{<<"content-type">> => <<"application/json">>},
        jsx:encode(Response), Req0),
    {ok, Req, State}.

iso8601_timestamp() ->
    {{Y, M, D}, {H, Mi, S}} = calendar:universal_time(),
    io_lib:format("~4..0B-~2..0B-~2..0BT~2..0B:~2..0B:~2..0BZ", [Y, M, D, H, Mi, S]).
