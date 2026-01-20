%%%-------------------------------------------------------------------
%%% @doc Desktop UI Application - Web-based interface for Mental Models
%%% 
%%% Serves a light-mode responsive web UI that connects to backend
%%% microservices via the API Gateway.
%%% 
%%% LICENSED SOFTWARE - Machine-specific authorization required
%%% @end
%%%-------------------------------------------------------------------
-module(desktop_ui_app).
-behaviour(application).

-export([start/2, stop/1]).

%% Authorized Machine GUIDs - Only these machines can run the software
-define(AUTHORIZED_MACHINES, [
    <<"a02b2fb2-1286-436d-9e5c-8d149f82dad9">>  %% Joel's primary machine
]).

start(_StartType, _StartArgs) ->
    %% Check license before starting
    case check_machine_authorization() of
        {ok, authorized} ->
            start_services();
        {error, unauthorized} ->
            io:format("~n"),
            io:format("========================================~n"),
            io:format("  UNAUTHORIZED MACHINE DETECTED~n"),
            io:format("========================================~n"),
            io:format("This software is licensed for specific machines only.~n"),
            io:format("Contact the administrator to authorize this machine.~n"),
            io:format("========================================~n~n"),
            {error, unauthorized_machine}
    end.

start_services() ->
    Port = application:get_env(desktop_ui, port, 3000),
    
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/", index_handler, []},
            {"/health", health_handler, []},
            {"/dashboard", dashboard_handler, []},
            {"/analysis", analysis_handler, []},
            {"/models", models_handler, []},
            {"/history", history_handler, []},
            {"/harvester", harvester_handler, []},
            {"/folder", folder_handler, []},
            {"/watcher", watcher_handler, []},
            {"/settings", settings_handler, []},
            {"/api/system/info", system_info_handler, []},
            {"/api/update/[...]", update_handler, []},
            {"/api/[...]", api_proxy_handler, []},
            {"/static/[...]", cowboy_static, {priv_dir, desktop_ui, "static"}},
            {"/[...]", not_found_handler, []}
        ]}
    ]),
    
    {ok, _} = cowboy:start_clear(http_listener, [{port, Port}],
        #{env => #{dispatch => Dispatch}}),
    
    io:format("Desktop UI started on port ~p~n", [Port]),
    io:format("Open http://localhost:~p in your browser~n", [Port]),
    desktop_ui_sup:start_link().

stop(_State) ->
    cowboy:stop_listener(http_listener).

%%====================================================================
%% License verification functions
%%====================================================================

check_machine_authorization() ->
    MachineId = get_machine_id(),
    io:format("[LICENSE] Checking machine authorization...~n"),
    io:format("[LICENSE] Machine ID: ~s~n", [MachineId]),
    
    case lists:member(MachineId, ?AUTHORIZED_MACHINES) of
        true ->
            io:format("[LICENSE] Machine AUTHORIZED~n"),
            {ok, authorized};
        false ->
            io:format("[LICENSE] Machine NOT AUTHORIZED~n"),
            {error, unauthorized}
    end.

get_machine_id() ->
    %% Try environment variable first (set by Docker from host)
    case os:getenv("MACHINE_GUID") of
        false ->
            %% Try Windows registry via PowerShell
            case get_windows_guid() of
                {ok, Guid} -> Guid;
                error ->
                    %% Try Linux machine-id
                    case get_linux_machine_id() of
                        {ok, Id} -> Id;
                        error ->
                            %% Fallback to hostname
                            case os:getenv("HOSTNAME") of
                                false -> <<"unknown">>;
                                H -> list_to_binary(H)
                            end
                    end
            end;
        Guid ->
            list_to_binary(Guid)
    end.

get_windows_guid() ->
    Cmd = "powershell -Command \"(Get-ItemProperty -Path 'HKLM:\\SOFTWARE\\Microsoft\\Cryptography' -Name MachineGuid -ErrorAction SilentlyContinue).MachineGuid\" 2>/dev/null",
    Result = string:trim(os:cmd(Cmd)),
    case Result of
        "" -> error;
        Guid when length(Guid) > 10 -> {ok, list_to_binary(Guid)};
        _ -> error
    end.

get_linux_machine_id() ->
    case file:read_file("/etc/machine-id") of
        {ok, Content} ->
            Id = string:trim(binary_to_list(Content)),
            {ok, list_to_binary(Id)};
        {error, _} ->
            error
    end.
