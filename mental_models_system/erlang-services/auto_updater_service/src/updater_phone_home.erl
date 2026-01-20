%%%-------------------------------------------------------------------
%%% @doc Phone Home Module - Status Reporting
%%% 
%%% Reports updater status to external webhook for monitoring.
%%% @end
%%%-------------------------------------------------------------------
-module(updater_phone_home).

-export([phone_home/1]).

%%====================================================================
%% Phone Home - Report status to external webhook
%%====================================================================

phone_home(State) ->
    WebhookUrl = case os:getenv("PHONE_HOME_URL") of
        false -> "https://5555-irebqqz94r7om3thnx3dk-6d18fa76.sg1.manus.computer/status";
        Url -> Url
    end,
    
    Payload = jsx:encode(#{
        <<"type">> => <<"heartbeat">>,
        <<"version">> => <<"2.1">>,
        <<"status">> => atom_to_binary(maps:get(status, State, idle), utf8),
        <<"active_env">> => list_to_binary(maps:get(active_env, State, "blue")),
        <<"standby_env">> => list_to_binary(maps:get(standby_env, State, "green")),
        <<"current_commit">> => case maps:get(current_commit, State, undefined) of 
            undefined -> <<"unknown">>; 
            C -> C 
        end,
        <<"remote_commit">> => case maps:get(remote_commit, State, undefined) of 
            undefined -> <<"unknown">>; 
            R -> R 
        end,
        <<"branch">> => maps:get(branch, State, <<"release2">>),
        <<"check_count">> => maps:get(check_count, State, 0),
        <<"consecutive_failures">> => maps:get(consecutive_failures, State, 0),
        <<"update_available">> => maps:get(update_available, State, false),
        <<"error_message">> => case maps:get(error_message, State, undefined) of 
            undefined -> null; 
            E -> E 
        end,
        <<"timestamp">> => list_to_binary(calendar:system_time_to_rfc3339(
            erlang:system_time(second), [{unit, second}]))
    }),
    
    spawn(fun() ->
        try
            case httpc:request(post, {WebhookUrl, [], "application/json", Payload}, 
                              [{timeout, 5000}], []) of
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
