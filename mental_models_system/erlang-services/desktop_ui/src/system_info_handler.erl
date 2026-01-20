%%%-------------------------------------------------------------------
%%% @doc System Info Handler - Returns system information including host path
%%% @end
%%%-------------------------------------------------------------------
-module(system_info_handler).
-behaviour(cowboy_handler).

-export([init/2]).

init(Req0, State) ->
    %% Get HOST_PATH from environment variable (set by START.bat)
    HostPath = case os:getenv("HOST_PATH") of
        false -> <<"Not set - run START.bat to set host path">>;
        Path -> list_to_binary(Path)
    end,
    
    %% Get MACHINE_GUID from environment variable
    MachineGuid = case os:getenv("MACHINE_GUID") of
        false -> <<"Not set">>;
        Guid -> list_to_binary(Guid)
    end,
    
    %% Build response
    Response = #{
        host_path => HostPath,
        machine_guid => MachineGuid,
        erlang_version => list_to_binary(erlang:system_info(otp_release)),
        node => atom_to_binary(node(), utf8)
    },
    
    Body = jsx:encode(Response),
    
    Req = cowboy_req:reply(200,
        #{<<"content-type">> => <<"application/json">>,
          <<"access-control-allow-origin">> => <<"*">>},
        Body,
        Req0),
    {ok, Req, State}.
