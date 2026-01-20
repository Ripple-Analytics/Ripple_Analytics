%%%-------------------------------------------------------------------
%%% @doc Backup Status Handler
%%% 
%%% Returns comprehensive status of the backup system.
%%% @end
%%%-------------------------------------------------------------------
-module(backup_status_handler).
-behaviour(cowboy_handler).

-export([init/2]).

init(Req0, State) ->
    %% Get status from all components
    GdriveStatus = case gdrive_client:get_status() of
        {ok, S1} -> S1;
        _ -> #{<<"error">> => <<"unavailable">>}
    end,
    
    ScheduleStatus = case backup_scheduler:get_schedule() of
        {ok, S2} -> S2;
        _ -> #{<<"error">> => <<"unavailable">>}
    end,
    
    BackupList = case gdrive_client:list_backups() of
        {ok, Backups} -> Backups;
        _ -> []
    end,
    
    Response = #{
        <<"gdrive">> => GdriveStatus,
        <<"schedule">> => ScheduleStatus,
        <<"backups">> => BackupList,
        <<"backup_count">> => length(BackupList),
        <<"timestamp">> => list_to_binary(iso8601_timestamp())
    },
    
    Req = cowboy_req:reply(200, 
        #{<<"content-type">> => <<"application/json">>,
          <<"access-control-allow-origin">> => <<"*">>},
        jsx:encode(Response), Req0),
    {ok, Req, State}.

iso8601_timestamp() ->
    {{Y, M, D}, {H, Mi, S}} = calendar:universal_time(),
    io_lib:format("~4..0B-~2..0B-~2..0BT~2..0B:~2..0B:~2..0BZ", [Y, M, D, H, Mi, S]).
