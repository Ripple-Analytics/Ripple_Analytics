# Security Implementation Guide for Mental Models System

This document provides comprehensive instructions for AI assistants and developers on how the codebase security is implemented and how to maintain/extend it.

## Overview

The Mental Models System uses a multi-layered security approach to ensure the software only runs on authorized machines while keeping the codebase public for transparency.

## Security Layers

### Layer 1: Machine-Specific License Verification

The software checks the machine's unique identifier (GUID) against an authorized list before starting any services.

**How it works:**

1. On Windows, the Machine GUID is read from the registry: `HKLM\SOFTWARE\Microsoft\Cryptography\MachineGuid`
2. On Linux, the machine ID is read from `/etc/machine-id`
3. The GUID is compared against the `AUTHORIZED_MACHINES` list in the code
4. If the machine is not authorized, the software displays an error and refuses to start

**Key Files:**

- `mental_models_system/erlang-services/desktop_ui/src/desktop_ui_app.erl` - Contains the license check logic and authorized machine list
- `mental_models_system/erlang-services/license_service/src/license_checker.erl` - Standalone license verification module

**To add a new authorized machine:**

1. Get the Machine GUID from the target machine:
   - Windows: Run `reg query "HKLM\SOFTWARE\Microsoft\Cryptography" /v MachineGuid` in Command Prompt
   - Linux: Run `cat /etc/machine-id`

2. Add the GUID to the `AUTHORIZED_MACHINES` list in `desktop_ui_app.erl`:
   ```erlang
   -define(AUTHORIZED_MACHINES, [
       <<"a02b2fb2-1286-436d-9e5c-8d149f82dad9">>,  %% Joel's primary machine
       <<"NEW-MACHINE-GUID-HERE">>                   %% Description of new machine
   ]).
   ```

3. Commit and push to the `release` branch - the auto-updater will deploy the change

### Layer 2: Environment Variable Passing

The Machine GUID is passed from the host machine to Docker containers via environment variables.

**How it works:**

1. `START.bat` (Windows) automatically reads the Machine GUID from the registry
2. The GUID is written to the `.env` file as `MACHINE_GUID=...`
3. `docker-compose.yml` passes this to containers: `MACHINE_GUID=${MACHINE_GUID:-}`
4. The Erlang application reads it via `os:getenv("MACHINE_GUID")`

**Key Files:**

- `mental_models_system/erlang-services/START.bat` - Windows startup script that extracts GUID
- `mental_models_system/erlang-services/docker-compose.yml` - Passes MACHINE_GUID to containers

### Layer 3: Auto-Updater Security

The auto-updater pulls code from the `release` branch only, which is controlled by authorized developers.

**How it works:**

1. Auto-updater checks GitHub every 5 minutes for new commits
2. Only pulls from the `release` branch (not master or other branches)
3. Rebuilds and restarts services when updates are detected
4. The auto-updater itself is "bulletproof" - it never stops running

**Key Files:**

- `mental_models_system/erlang-services/auto_updater_service/src/updater_worker.erl` - Core update logic

## Security Best Practices for AI Assistants

### DO:

1. Always add new authorized machines to the `AUTHORIZED_MACHINES` list
2. Push security-related changes to the `release` branch
3. Test license verification locally before deploying
4. Keep the authorized machine list minimal
5. Document each authorized machine with a comment

### DON'T:

1. Never remove the license check from `desktop_ui_app.erl`
2. Never push to master branch directly (it's protected)
3. Never expose the Machine GUIDs in logs or error messages to end users
4. Never disable the auto-updater's bulletproof mechanisms
5. Never add wildcard or bypass logic to the license check

## Adding New Security Layers

If you need to add additional security:

### Option A: Add IP-based restrictions

Add to `desktop_ui_app.erl`:
```erlang
-define(AUTHORIZED_IPS, [<<"192.168.1.100">>, <<"10.0.0.50">>]).

check_ip_authorization() ->
    %% Get local IP and compare against authorized list
    ...
```

### Option B: Add time-based license expiration

Add to the license check:
```erlang
-define(LICENSE_EXPIRY, {{2027, 1, 1}, {0, 0, 0}}).

check_license_expiry() ->
    Now = calendar:universal_time(),
    Now < ?LICENSE_EXPIRY.
```

### Option C: Add hardware fingerprinting

Combine multiple identifiers:
```erlang
get_hardware_fingerprint() ->
    MachineGuid = get_machine_id(),
    CpuId = get_cpu_id(),
    crypto:hash(sha256, <<MachineGuid/binary, CpuId/binary>>).
```

## Troubleshooting

### "Unauthorized Machine" Error

1. Check the Machine GUID is correct: `reg query "HKLM\SOFTWARE\Microsoft\Cryptography" /v MachineGuid`
2. Verify the GUID is in the `AUTHORIZED_MACHINES` list
3. Ensure the `.env` file contains the correct `MACHINE_GUID=...`
4. Restart Docker containers after updating `.env`

### Auto-Updater Not Picking Up Changes

1. Check auto-updater logs: `docker logs -f mental-models-auto-updater`
2. Verify changes are pushed to `release` branch (not master)
3. Force restart: `docker restart mental-models-auto-updater`

### License Check Bypassed

If the license check appears to be bypassed:
1. Verify `desktop_ui_app.erl` contains the `check_machine_authorization()` call in `start/2`
2. Check that `AUTHORIZED_MACHINES` list is not empty
3. Ensure Docker image was rebuilt after code changes

## Current Authorized Machines

| Machine GUID | Owner | Description |
|--------------|-------|-------------|
| a02b2fb2-1286-436d-9e5c-8d149f82dad9 | Joel | Primary development machine |

## Version History

- v1.0.0 (2026-01-20): Initial machine-specific license verification
- v1.1.0 (2026-01-20): Added environment variable passing via Docker
- v1.2.0 (2026-01-20): Added bulletproof auto-updater with heartbeat/watchdog
