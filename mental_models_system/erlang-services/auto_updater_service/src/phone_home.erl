%%%-------------------------------------------------------------------
%%% @doc Phone Home - Report status to external webhook
%%% @end
%%%-------------------------------------------------------------------
-module(phone_home).

-export([report/1]).

-define(DEFAULT_URL, "https://webhook.example.com/status").

%% @doc Send status report to webhook (async, fire-and-forget)
report(Status) when is_map(Status) ->
    spawn(fun() -> do_report(Status) end).

%% Internal: perform the HTTP POST
do_report(Status) ->
    Url = get_webhook_url(),
    Payload = build_payload(Status),
    try
        case httpc:request(post, {Url, [], "application/json", Payload}, [{timeout, 5000}], []) of
            {ok, {{_, 200, _}, _, _}} -> ok;
            {ok, {{_, Code, _}, _, _}} -> 
                io:format("[PHONE_HOME] HTTP ~p~n", [Code]);
            {error, Reason} -> 
                io:format("[PHONE_HOME] Failed: ~p~n", [Reason])
        end
    catch
        _:_ -> ok
    end.

%% Internal: get webhook URL from env or default
get_webhook_url() ->
    case os:getenv("PHONE_HOME_URL") of
        false -> ?DEFAULT_URL;
        Url -> Url
    end.

%% Internal: build JSON payload
build_payload(Status) ->
    jsx:encode(#{
        <<"type">> => <<"heartbeat">>,
        <<"version">> => <<"2.1">>,
        <<"status">> => maps:get(status, Status, <<"unknown">>),
        <<"active_env">> => maps:get(active_env, Status, <<"unknown">>),
        <<"current_commit">> => maps:get(current_commit, Status, <<"unknown">>),
        <<"remote_commit">> => maps:get(remote_commit, Status, <<"unknown">>),
        <<"check_count">> => maps:get(check_count, Status, 0),
        <<"failures">> => maps:get(failures, Status, 0),
        <<"timestamp">> => timestamp()
    }).

%% Internal: current timestamp
timestamp() ->
    list_to_binary(calendar:system_time_to_rfc3339(
        erlang:system_time(second), [{unit, second}])).
