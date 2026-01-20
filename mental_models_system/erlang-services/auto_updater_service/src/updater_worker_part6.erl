%%%-------------------------------------------------------------------
%%% @doc updater_worker Helper Module - Part 6
%%% @end
%%%-------------------------------------------------------------------
-module(updater_worker_part6).

-export([phone_home/1]).

phone_home(State) ->
    %% Get webhook URL from environment or use hardcoded fallback
    WebhookUrl = case os:getenv("PHONE_HOME_URL") of
        false -> "https://5555-irebqqz94r7om3thnx3dk-6d18fa76.sg1.manus.computer/status";
        Url -> Url
    end,
    
    %% Build status payload
    Payload = jsx:encode(#{
        <<"type">> => <<"heartbeat">>,
        <<"version">> => <<"2.1">>,
        <<"status">> => atom_to_binary(State#state.status, utf8),
        <<"active_env">> => list_to_binary(State#state.active_env),
        <<"standby_env">> => list_to_binary(State#state.standby_env),
        <<"current_commit">> => case State#state.current_commit of undefined -> <<"unknown">>; C -> C end,
        <<"remote_commit">> => case State#state.remote_commit of undefined -> <<"unknown">>; R -> R end,
        <<"branch">> => State#state.branch,
        <<"check_count">> => State#state.check_count,
        <<"consecutive_failures">> => State#state.consecutive_failures,
        <<"update_available">> => State#state.update_available,
        <<"error_message">> => case State#state.error_message of undefined -> null; E -> E end,
        <<"timestamp">> => list_to_binary(calendar:system_time_to_rfc3339(erlang:system_time(second), [{unit, second}]))
    }),
    
    %% Send HTTP POST (fire and forget, don't block on errors)
    spawn(fun() ->
        try
            case httpc:request(post, {WebhookUrl, [], "application/json", Payload}, [{timeout, 5000}], []) of
                {ok, {{_, 200, _}, _, _}} ->
                    io:format("[UPDATER] Phone home: OK~n");
                {ok, {{_, Code, _}, _, _}} ->
                    io:format("[UPDATER] Phone home: HTTP ~p~n", [Code]);
                {error, Reason} ->
                    io:format("[UPDATER] Phone home failed: ~p~n", [Reason])
            end
        catch
            _:_ -> ok
        end
    end).

