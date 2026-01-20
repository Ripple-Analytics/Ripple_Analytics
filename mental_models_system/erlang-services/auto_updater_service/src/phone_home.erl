%%%-------------------------------------------------------------------
%%% @doc Phone Home - Report status to external webhook
%%% @end
%%%-------------------------------------------------------------------
-module(phone_home).

-export([report/1]).

-define(DEFAULT_URL, "https://webhook.example.com/status").

%% @doc Send status report to webhook (async, fire-and-forget)
report(Status) when is_map(Status) ->
    spawn(fun() -> do_report(Status) end);
report(_) -> ok.

%% Internal: perform the HTTP POST
do_report(Status) ->
    try
        ensure_inets(),
        Url = get_webhook_url(),
        Payload = build_payload(Status),
        case httpc:request(post, {Url, [], "application/json", Payload}, 
                          [{timeout, 5000}, {connect_timeout, 3000}], []) of
            {ok, {{_, Code, _}, _, _}} when Code >= 200, Code < 300 -> ok;
            {ok, {{_, Code, _}, _, _}} -> 
                io:format("[PHONE_HOME] HTTP ~p~n", [Code]);
            {error, Reason} -> 
                io:format("[PHONE_HOME] Failed: ~p~n", [Reason])
        end
    catch
        Class:Error ->
            io:format("[PHONE_HOME] Error: ~p:~p~n", [Class, Error])
    end.

%% Internal: ensure inets is started
ensure_inets() ->
    case application:ensure_all_started(inets) of
        {ok, _} -> ok;
        {error, {already_started, _}} -> ok;
        _ -> ok
    end.

%% Internal: get webhook URL from env or default
get_webhook_url() ->
    case os:getenv("PHONE_HOME_URL") of
        false -> ?DEFAULT_URL;
        Url -> Url
    end.

%% Internal: build JSON payload
build_payload(Status) ->
    try
        jsx:encode(#{
            <<"type">> => <<"heartbeat">>,
            <<"version">> => <<"3.0">>,
            <<"status">> => safe_get(status, Status),
            <<"active_env">> => safe_get(active_env, Status),
            <<"current_commit">> => safe_get(current_commit, Status),
            <<"remote_commit">> => safe_get(remote_commit, Status),
            <<"check_count">> => maps:get(check_count, Status, 0),
            <<"failures">> => maps:get(failures, Status, 0),
            <<"timestamp">> => timestamp()
        })
    catch
        _:_ -> "{\"error\":\"encode_failed\"}"
    end.

%% Internal: safely get value from map
safe_get(Key, Map) ->
    case maps:get(Key, Map, undefined) of
        undefined -> <<"unknown">>;
        Val when is_atom(Val) -> atom_to_binary(Val, utf8);
        Val when is_binary(Val) -> Val;
        Val when is_list(Val) -> list_to_binary(Val);
        _ -> <<"unknown">>
    end.

%% Internal: current timestamp
timestamp() ->
    list_to_binary(calendar:system_time_to_rfc3339(
        erlang:system_time(second), [{unit, second}])).
