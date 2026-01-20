%%%-------------------------------------------------------------------
%%% @doc lm_client Helper Module - Part 2
%%% @end
%%%-------------------------------------------------------------------
-module(lm_client_part2).

-export([handle_info/2, handle_info/2, terminate/2, get_lm_studio_url/0, ensure_binary/1, ensure_binary/1, do_request_with_retry/3, do_request_with_retry/3, update_stats/3, calculate_success_rate/1, calculate_success_rate/2]).

handle_info(check_initial_connection, State) ->
    Url = State#state.base_url ++ "/v1/models",
    Connected = case hackney:request(get, list_to_binary(Url), [], <<>>, [{timeout, 5000}]) of
        {ok, 200, _Headers, ClientRef} ->
            hackney:body(ClientRef),
            io:format("[LM-CLIENT] Initial connection successful~n"),
            true;
        _ ->
            io:format("[LM-CLIENT] Initial connection failed - will retry on requests~n"),
            false
    end,
    {noreply, State#state{connected = Connected, last_check = erlang:system_time(second)}};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================

get_lm_studio_url() ->
    case os:getenv("LM_STUDIO_URL") of
        false -> "http://host.docker.internal:1234";
        Url -> Url
    end.

ensure_binary(Bin) when is_binary(Bin) -> Bin;
ensure_binary(List) when is_list(List) -> list_to_binary(List).

do_request_with_retry(_Url, _Body, 0) ->
    {error, max_retries_exceeded};
do_request_with_retry(Url, Body, AttemptsLeft) ->
    case hackney:request(post, list_to_binary(Url), 
                         [{<<"Content-Type">>, <<"application/json">>}],
                         Body, 
                         [{timeout, ?DEFAULT_TIMEOUT}, {recv_timeout, ?DEFAULT_TIMEOUT}]) of
        {ok, 200, _Headers, ClientRef} ->
            {ok, ResponseBody} = hackney:body(ClientRef),
            Response = jsx:decode(ResponseBody, [return_maps]),
            Choices = maps:get(<<"choices">>, Response, []),
            case Choices of
                [FirstChoice | _] ->
                    Message = maps:get(<<"message">>, FirstChoice, #{}),
                    Content = maps:get(<<"content">>, Message, <<>>),
                    {ok, Content};
                [] ->
                    {error, no_response}
            end;
        {ok, StatusCode, _Headers, ClientRef} ->
            {ok, ErrorBody} = hackney:body(ClientRef),
            io:format("[LM-CLIENT] HTTP ~p, retrying (~p left)...~n", [StatusCode, AttemptsLeft - 1]),
            timer:sleep(?RETRY_DELAY),
            do_request_with_retry(Url, Body, AttemptsLeft - 1);
        {error, Reason} ->
            io:format("[LM-CLIENT] Request error: ~p, retrying (~p left)...~n", [Reason, AttemptsLeft - 1]),
            timer:sleep(?RETRY_DELAY),
            do_request_with_retry(Url, Body, AttemptsLeft - 1)
    end.

update_stats(State, Result, ResponseTime) ->
    NewTotal = State#state.total_requests + 1,
    {NewSuccessful, NewFailed} = case Result of
        {ok, _} -> {State#state.successful_requests + 1, State#state.failed_requests};
        {error, _} -> {State#state.successful_requests, State#state.failed_requests + 1}
    end,
    
    %% Calculate running average response time
    OldAvg = State#state.avg_response_time,
    NewAvg = case State#state.total_requests of
        0 -> float(ResponseTime);
        N -> (OldAvg * N + ResponseTime) / (N + 1)
    end,
    
    State#state{
        total_requests = NewTotal,
        successful_requests = NewSuccessful,
        failed_requests = NewFailed,
        avg_response_time = NewAvg
    }.

calculate_success_rate(#state{total_requests = 0}) -> 100.0;
calculate_success_rate(#state{successful_requests = S, total_requests = T}) ->
    (S / T) * 100.

