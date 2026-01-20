%%%-------------------------------------------------------------------
%%% @doc Error Aggregator
%%% Collects, deduplicates, and prioritizes errors from all services.
%%% Only triggers fixes for new/unique errors, not duplicates.
%%% @end
%%%-------------------------------------------------------------------
-module(error_aggregator).
-behaviour(gen_server).

-export([start_link/0, init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).
-export([add_error/1, get_errors/0, get_unfixed_errors/0, mark_fixed/1, clear_old/0]).

-record(state, {
    errors = [],           %% List of unique errors
    error_hashes = #{},    %% Hash -> error for deduplication
    fixed_hashes = #{},    %% Hashes of errors that have been fixed
    max_errors = 1000,     %% Max errors to keep
    error_ttl = 3600000    %% 1 hour TTL for errors
}).

%%====================================================================
%% API
%%====================================================================

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

add_error(Error) ->
    gen_server:cast(?MODULE, {add_error, Error}).

get_errors() ->
    gen_server:call(?MODULE, get_errors).

get_unfixed_errors() ->
    gen_server:call(?MODULE, get_unfixed_errors).

mark_fixed(ErrorHash) ->
    gen_server:cast(?MODULE, {mark_fixed, ErrorHash}).

clear_old() ->
    gen_server:cast(?MODULE, clear_old).

%%====================================================================
%% gen_server callbacks
%%====================================================================

init([]) ->
    io:format("[ERROR-AGG] Starting error aggregator~n"),
    %% Clean old errors every 10 minutes
    erlang:send_after(600000, self(), cleanup_timer),
    {ok, #state{}}.

handle_call(get_errors, _From, State) ->
    {reply, {ok, State#state.errors}, State};

handle_call(get_unfixed_errors, _From, State) ->
    Unfixed = lists:filter(fun(Error) ->
        Hash = error_hash(Error),
        not maps:is_key(Hash, State#state.fixed_hashes)
    end, State#state.errors),
    {reply, {ok, Unfixed}, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown}, State}.

handle_cast({add_error, Error}, State) ->
    Hash = error_hash(Error),
    
    %% Check if we've seen this error before
    case maps:is_key(Hash, State#state.error_hashes) of
        true ->
            %% Duplicate, ignore
            {noreply, State};
        false ->
            %% New error
            io:format("[ERROR-AGG] New error from ~s: ~s~n", 
                      [maps:get(service, Error, <<"unknown">>),
                       maps:get(type, Error, <<"unknown">>)]),
            
            NewErrors = [Error | State#state.errors],
            NewHashes = maps:put(Hash, Error, State#state.error_hashes),
            
            %% Trigger fix pipeline for unfixed errors only
            case maps:is_key(Hash, State#state.fixed_hashes) of
                false ->
                    fix_pipeline:queue_error(Error);
                true ->
                    ok  %% Already fixed this type of error
            end,
            
            %% Trim if too many errors
            TrimmedErrors = case length(NewErrors) > State#state.max_errors of
                true -> lists:sublist(NewErrors, State#state.max_errors);
                false -> NewErrors
            end,
            
            {noreply, State#state{errors = TrimmedErrors, error_hashes = NewHashes}}
    end;

handle_cast({mark_fixed, ErrorHash}, State) ->
    io:format("[ERROR-AGG] Marking error as fixed: ~s~n", [ErrorHash]),
    NewFixed = maps:put(ErrorHash, erlang:timestamp(), State#state.fixed_hashes),
    {noreply, State#state{fixed_hashes = NewFixed}};

handle_cast(clear_old, State) ->
    NewState = do_cleanup(State),
    {noreply, NewState};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(cleanup_timer, State) ->
    NewState = do_cleanup(State),
    erlang:send_after(600000, self(), cleanup_timer),
    {noreply, NewState};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================

error_hash(Error) ->
    %% Create a hash based on service + error type + key part of message
    Service = maps:get(service, Error, <<"unknown">>),
    Type = maps:get(type, Error, <<"unknown">>),
    Line = maps:get(line, Error, <<"">>),
    %% Take first 100 chars of line for hash
    LineKey = binary:part(Line, 0, min(100, byte_size(Line))),
    erlang:md5(<<Service/binary, Type/binary, LineKey/binary>>).

do_cleanup(State) ->
    Now = erlang:system_time(millisecond),
    TTL = State#state.error_ttl,
    
    %% Remove errors older than TTL
    FilteredErrors = lists:filter(fun(Error) ->
        Timestamp = maps:get(timestamp, Error, {0, 0, 0}),
        ErrorTime = timer:now_diff(Now, Timestamp) div 1000,
        ErrorTime < TTL
    end, State#state.errors),
    
    io:format("[ERROR-AGG] Cleanup: ~p -> ~p errors~n", 
              [length(State#state.errors), length(FilteredErrors)]),
    
    State#state{errors = FilteredErrors}.
