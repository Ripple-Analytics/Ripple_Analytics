%%%-------------------------------------------------------------------
%%% @doc Notification Service - Real-time Analysis Alerts
%%% 
%%% Provides real-time notifications for analysis events using
%%% Server-Sent Events (SSE). Notifies clients when:
%%% - Lollapalooza effects are detected
%%% - High-scoring mental models are found
%%% - Folder watcher processes new files
%%% @end
%%%-------------------------------------------------------------------
-module(notification_service).
-behaviour(gen_server).

-export([start_link/0, notify/2, subscribe/1, unsubscribe/1, get_recent/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-define(SERVER, ?MODULE).
-define(MAX_RECENT, 50).

-record(state, {
    subscribers = [] :: [pid()],
    recent = [] :: [map()]
}).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

notify(Type, Data) ->
    gen_server:cast(?SERVER, {notify, Type, Data}).

subscribe(Pid) ->
    gen_server:call(?SERVER, {subscribe, Pid}).

unsubscribe(Pid) ->
    gen_server:cast(?SERVER, {unsubscribe, Pid}).

get_recent() ->
    gen_server:call(?SERVER, get_recent).

init([]) ->
    io:format("Notification Service started~n"),
    {ok, #state{}}.

handle_call({subscribe, Pid}, _From, State = #state{subscribers = Subs}) ->
    monitor(process, Pid),
    {reply, ok, State#state{subscribers = [Pid | Subs]}};

handle_call(get_recent, _From, State = #state{recent = Recent}) ->
    {reply, {ok, Recent}, State};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({notify, Type, Data}, State = #state{subscribers = Subs, recent = Recent}) ->
    Timestamp = format_timestamp(),
    Notification = #{
        <<"type">> => atom_to_binary(Type, utf8),
        <<"data">> => Data,
        <<"timestamp">> => Timestamp,
        <<"id">> => generate_id()
    },
    
    lists:foreach(fun(Pid) ->
        Pid ! {notification, Notification}
    end, Subs),
    
    NewRecent = lists:sublist([Notification | Recent], ?MAX_RECENT),
    {noreply, State#state{recent = NewRecent}};

handle_cast({unsubscribe, Pid}, State = #state{subscribers = Subs}) ->
    {noreply, State#state{subscribers = lists:delete(Pid, Subs)}};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({'DOWN', _Ref, process, Pid, _Reason}, State = #state{subscribers = Subs}) ->
    {noreply, State#state{subscribers = lists:delete(Pid, Subs)}};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

format_timestamp() ->
    {{Y, M, D}, {H, Mi, S}} = calendar:local_time(),
    list_to_binary(io_lib:format("~4..0B-~2..0B-~2..0B ~2..0B:~2..0B:~2..0B", [Y, M, D, H, Mi, S])).

generate_id() ->
    list_to_binary(integer_to_list(erlang:unique_integer([positive]))).
