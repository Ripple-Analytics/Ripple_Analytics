%%%-------------------------------------------------------------------
%%% @doc Update Handler - Manages auto-updater controls and configuration
%%% 
%%% Provides API endpoints for:
%%% - Checking update status
%%% - Triggering manual updates
%%% - Configuring update sources (GitHub token, Google Drive URL)
%%% @end
%%%-------------------------------------------------------------------
-module(update_handler).
%% Helper modules: update_handler_part2
-behaviour(cowboy_handler).

-export([init/2]).

-define(STATUS_FILE, "/data/updater_status.json").
-define(CONFIG_FILE, "/data/updater_config.json").

init(Req0, State) ->
    Method = cowboy_req:method(Req0),
    Path = cowboy_req:path(Req0),
    
    case {Method, Path} of
        {<<"GET">>, <<"/api/update/status">>} ->
            handle_get_status(Req0, State);
        {<<"POST">>, <<"/api/update/trigger">>} ->
            handle_trigger_update(Req0, State);
        {<<"GET">>, <<"/api/update/config">>} ->
            handle_get_config(Req0, State);
        {<<"POST">>, <<"/api/update/config">>} ->
            handle_save_config(Req0, State);
        {<<"POST">>, <<"/api/update/restart">>} ->
            handle_restart_updater(Req0, State);
        {<<"OPTIONS">>, _} ->
            Req = cowboy_req:reply(200, cors_headers(), <<>>, Req0),
            {ok, Req, State};
        _ ->
            Req = cowboy_req:reply(404, cors_headers(),
                jsx:encode(#{<<"error">> => <<"Not found">>}), Req0),
            {ok, Req, State}
    end.

%% Get current update status
handle_get_status(Req0, State) ->
    Status = case file:read_file(?STATUS_FILE) of
        {ok, Content} ->
            try jsx:decode(Content, [return_maps]) of
                Data -> Data
            catch
                _:_ -> default_status()
            end;
        {error, _} ->
            default_status()
    end,
    
    %% Add version info
    StatusWithVersion = Status#{
        <<"version">> => <<"1.1.0">>,
        <<"erlang_version">> => list_to_binary(erlang:system_info(otp_release))
    },
    
    Req = cowboy_req:reply(200, cors_headers(),
        jsx:encode(StatusWithVersion), Req0),
    {ok, Req, State}.

%% Trigger manual update check
handle_trigger_update(Req0, State) ->
    %% Signal the auto-updater to check for updates immediately
    %% We do this by creating a trigger file that the updater watches
    TriggerFile = "/data/update_trigger",
    file:write_file(TriggerFile, <<"check">>),
    
    %% Also try to restart the auto-updater container to force immediate check
    Result = os:cmd("docker restart mental-models-auto-updater 2>&1"),
    
    Response = #{
        <<"success">> => true,
        <<"message">> => <<"Update check triggered. The auto-updater will check for updates shortly.">>,
        <<"details">> => list_to_binary(Result)
    },
    
    Req = cowboy_req:reply(200, cors_headers(),
        jsx:encode(Response), Req0),
    {ok, Req, State}.

%% Get current configuration (without exposing sensitive tokens)
handle_get_config(Req0, State) ->
    Config = case file:read_file(?CONFIG_FILE) of
        {ok, Content} ->
            try jsx:decode(Content, [return_maps]) of
                Data -> 
                    %% Mask sensitive data
                    mask_sensitive(Data)
            catch
                _:_ -> default_config()
            end;
        {error, _} ->
            default_config()
    end,
    
    Req = cowboy_req:reply(200, cors_headers(),
        jsx:encode(Config), Req0),
    {ok, Req, State}.

%% Save configuration
handle_save_config(Req0, State) ->
    {ok, Body, Req1} = cowboy_req:read_body(Req0),
    
    try jsx:decode(Body, [return_maps]) of
        NewConfig ->
            %% Merge with existing config
            ExistingConfig = case file:read_file(?CONFIG_FILE) of
                {ok, Content} ->
                    try jsx:decode(Content, [return_maps]) catch _:_ -> #{} end;
                {error, _} -> #{}
            end,
            
            MergedConfig = maps:merge(ExistingConfig, NewConfig),
            
            %% Save to file
            ok = filelib:ensure_dir(?CONFIG_FILE),
            ok = file:write_file(?CONFIG_FILE, jsx:encode(MergedConfig)),
            
            %% Also write to .env file for the auto-updater
            write_env_file(MergedConfig),
            
            Response = #{
                <<"success">> => true,
                <<"message">> => <<"Configuration saved. Restart the auto-updater to apply changes.">>
            },
            
            Req = cowboy_req:reply(200, cors_headers(),
                jsx:encode(Response), Req1),
            {ok, Req, State}
    catch
        _:_ ->
            Req2 = cowboy_req:reply(400, cors_headers(),
                jsx:encode(#{<<"error">> => <<"Invalid JSON">>}), Req1),
            {ok, Req2, State}
    end.

%% Restart the auto-updater container