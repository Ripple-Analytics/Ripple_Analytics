%%%-------------------------------------------------------------------
%%% @doc License Checker - Machine-specific license verification
%%% This module verifies that the software is running on an authorized machine.
%%% Unauthorized machines will not be able to run the software.
%%% @end
%%%-------------------------------------------------------------------
-module(license_checker).

-export([check_license/0, is_authorized/0, get_machine_id/0, get_authorized_machines/0]).

%% Authorized Machine GUIDs (Windows Machine GUIDs)
%% Add new machines here to authorize them
-define(AUTHORIZED_MACHINES, [
    <<"a02b2fb2-1286-436d-9e5c-8d149f82dad9">>  %% Joel's primary machine
]).

%% License key for encryption/decryption (keep this secret)
-define(LICENSE_KEY, <<"MentalModels2026SecureKey!@#$%^&*">>).

%%====================================================================
%% API
%%====================================================================

%% Check license and return result
check_license() ->
    case is_authorized() of
        true ->
            io:format("[LICENSE] Machine authorized - starting services~n"),
            {ok, authorized};
        false ->
            MachineId = get_machine_id(),
            io:format("[LICENSE] ERROR: Unauthorized machine detected!~n"),
            io:format("[LICENSE] Machine ID: ~s~n", [MachineId]),
            io:format("[LICENSE] Contact administrator to authorize this machine.~n"),
            {error, unauthorized}
    end.

%% Check if current machine is authorized
is_authorized() ->
    MachineId = get_machine_id(),
    AuthorizedMachines = get_authorized_machines(),
    lists:member(MachineId, AuthorizedMachines).

%% Get the current machine's unique identifier
get_machine_id() ->
    %% Try multiple methods to get machine ID
    case get_windows_machine_guid() of
        {ok, Guid} -> Guid;
        error ->
            case get_linux_machine_id() of
                {ok, Id} -> Id;
                error ->
                    case get_docker_hostname() of
                        {ok, Hostname} -> Hostname;
                        error -> <<"unknown">>
                    end
            end
    end.

%% Get list of authorized machines
get_authorized_machines() ->
    ?AUTHORIZED_MACHINES.

%%====================================================================
%% Internal functions
%%====================================================================

%% Get Windows Machine GUID from registry
get_windows_machine_guid() ->
    %% Try to read from environment variable first (set by Docker)
    case os:getenv("MACHINE_GUID") of
        false ->
            %% Try to read from Windows registry via PowerShell
            Cmd = "powershell -Command \"(Get-ItemProperty -Path 'HKLM:\\SOFTWARE\\Microsoft\\Cryptography' -Name MachineGuid -ErrorAction SilentlyContinue).MachineGuid\" 2>/dev/null",
            Result = string:trim(os:cmd(Cmd)),
            case Result of
                "" -> error;
                Guid when length(Guid) > 10 -> {ok, list_to_binary(Guid)};
                _ -> error
            end;
        Guid ->
            {ok, list_to_binary(Guid)}
    end.

%% Get Linux machine ID
get_linux_machine_id() ->
    case file:read_file("/etc/machine-id") of
        {ok, Content} ->
            Id = string:trim(binary_to_list(Content)),
            {ok, list_to_binary(Id)};
        {error, _} ->
            %% Try alternative location
            case file:read_file("/var/lib/dbus/machine-id") of
                {ok, Content2} ->
                    Id2 = string:trim(binary_to_list(Content2)),
                    {ok, list_to_binary(Id2)};
                {error, _} ->
                    error
            end
    end.

%% Get Docker container hostname (fallback)
get_docker_hostname() ->
    case os:getenv("HOSTNAME") of
        false -> error;
        Hostname -> {ok, list_to_binary(Hostname)}
    end.

%% Encrypt data with license key (for future use)
encrypt(Data) when is_binary(Data) ->
    Key = crypto:hash(sha256, ?LICENSE_KEY),
    IV = crypto:strong_rand_bytes(16),
    Encrypted = crypto:crypto_one_time(aes_256_cbc, Key, IV, Data, true),
    <<IV/binary, Encrypted/binary>>.

%% Decrypt data with license key (for future use)
decrypt(<<IV:16/binary, Encrypted/binary>>) ->
    Key = crypto:hash(sha256, ?LICENSE_KEY),
    crypto:crypto_one_time(aes_256_cbc, Key, IV, Encrypted, false).
