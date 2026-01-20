%%%-------------------------------------------------------------------
%%% @doc Backup List Handler
%%% 
%%% Lists available backups.
%%% @end
%%%-------------------------------------------------------------------
-module(backup_list_handler).
-behaviour(cowboy_handler).

-export([init/2]).

init(Req0, State) ->
    Result = gdrive_client:list_backups(),
    
    Response = case Result of
        {ok, Backups} ->
            #{<<"success">> => true, <<"backups">> => Backups, <<"count">> => length(Backups)};
        {error, Reason} ->
            #{<<"success">> => false, <<"error">> => list_to_binary(io_lib:format("~p", [Reason]))}
    end,
    
    Req = cowboy_req:reply(200, 
        #{<<"content-type">> => <<"application/json">>,
          <<"access-control-allow-origin">> => <<"*">>},
        jsx:encode(Response), Req0),
    {ok, Req, State}.
