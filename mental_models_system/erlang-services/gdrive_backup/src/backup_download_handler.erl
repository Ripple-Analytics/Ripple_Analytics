%%%-------------------------------------------------------------------
%%% @doc Backup Download Handler
%%% 
%%% Allows downloading backup files.
%%% @end
%%%-------------------------------------------------------------------
-module(backup_download_handler).
-behaviour(cowboy_handler).

-export([init/2]).

init(Req0, State) ->
    BackupId = cowboy_req:binding(id, Req0),
    
    BackupPath = "/data/backups/" ++ binary_to_list(BackupId),
    
    case filelib:is_regular(BackupPath) of
        true ->
            {ok, Content} = file:read_file(BackupPath),
            
            Req = cowboy_req:reply(200, 
                #{<<"content-type">> => <<"application/octet-stream">>,
                  <<"content-disposition">> => <<"attachment; filename=\"", BackupId/binary, "\"">>,
                  <<"access-control-allow-origin">> => <<"*">>},
                Content, Req0),
            {ok, Req, State};
        false ->
            Req = cowboy_req:reply(404, 
                #{<<"content-type">> => <<"application/json">>},
                jsx:encode(#{<<"error">> => <<"Backup not found">>}), Req0),
            {ok, Req, State}
    end.
