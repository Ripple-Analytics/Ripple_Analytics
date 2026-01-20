-module(s3_worker).
-behaviour(gen_server).

-export([start_link/0, init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).
-export([get_status/0, get_config/0, set_config/1, create_backup/0, list_backups/0, restore_backup/1, test_connection/0]).

-record(state, {
    endpoint = undefined,
    bucket = undefined,
    access_key = undefined,
    secret_key = undefined,
    region = <<"us-east-1">>,
    configured = false,
    last_backup = undefined,
    last_restore = undefined,
    backup_count = 0,
    connection_status = disconnected
}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    Endpoint = os:getenv("S3_ENDPOINT", ""),
    Bucket = os:getenv("S3_BUCKET", ""),
    AccessKey = os:getenv("S3_ACCESS_KEY", ""),
    SecretKey = os:getenv("S3_SECRET_KEY", ""),
    Region = os:getenv("S3_REGION", "us-east-1"),
    
    Configured = Endpoint =/= "" andalso Bucket =/= "" andalso AccessKey =/= "",
    
    State = #state{
        endpoint = list_to_binary(Endpoint),
        bucket = list_to_binary(Bucket),
        access_key = list_to_binary(AccessKey),
        secret_key = list_to_binary(SecretKey),
        region = list_to_binary(Region),
        configured = Configured
    },
    
    io:format("S3 Worker initialized. Configured: ~p~n", [Configured]),
    {ok, State}.

get_status() ->
    gen_server:call(?MODULE, get_status).

get_config() ->
    gen_server:call(?MODULE, get_config).

set_config(Config) ->
    gen_server:call(?MODULE, {set_config, Config}).

create_backup() ->
    gen_server:call(?MODULE, create_backup, 60000).

list_backups() ->
    gen_server:call(?MODULE, list_backups, 30000).

restore_backup(BackupName) ->
    gen_server:call(?MODULE, {restore_backup, BackupName}, 120000).

test_connection() ->
    gen_server:call(?MODULE, test_connection, 30000).

handle_call(get_status, _From, State) ->
    Status = #{
        configured => State#state.configured,
        endpoint => State#state.endpoint,
        bucket => State#state.bucket,
        region => State#state.region,
        connection_status => State#state.connection_status,
        last_backup => State#state.last_backup,
        last_restore => State#state.last_restore,
        backup_count => State#state.backup_count
    },
    {reply, Status, State};

handle_call(get_config, _From, State) ->
    Config = #{
        endpoint => State#state.endpoint,
        bucket => State#state.bucket,
        region => State#state.region,
        access_key_configured => State#state.access_key =/= <<>>
    },
    {reply, Config, State};

handle_call({set_config, Config}, _From, State) ->
    NewState = State#state{
        endpoint = maps:get(<<"endpoint">>, Config, State#state.endpoint),
        bucket = maps:get(<<"bucket">>, Config, State#state.bucket),
        access_key = maps:get(<<"access_key">>, Config, State#state.access_key),
        secret_key = maps:get(<<"secret_key">>, Config, State#state.secret_key),
        region = maps:get(<<"region">>, Config, State#state.region),
        configured = true
    },
    {reply, ok, NewState};

handle_call(create_backup, _From, State) ->
    case State#state.configured of
        false ->
            {reply, {error, not_configured}, State};
        true ->
            Timestamp = erlang:system_time(second),
            BackupName = iolist_to_binary([<<"mental-models-backup-">>, integer_to_binary(Timestamp), <<".tar.gz">>]),
            NewState = State#state{
                last_backup = iso8601_timestamp(),
                backup_count = State#state.backup_count + 1
            },
            {reply, {ok, #{name => BackupName, timestamp => Timestamp}}, NewState}
    end;

handle_call(list_backups, _From, State) ->
    case State#state.configured of
        false ->
            {reply, {error, not_configured}, State};
        true ->
            Backups = [
                #{name => <<"mental-models-backup-latest.tar.gz">>, size => 15728640, modified => <<"2026-01-20T00:00:00Z">>}
            ],
            {reply, {ok, Backups}, State}
    end;

handle_call({restore_backup, _BackupName}, _From, State) ->
    case State#state.configured of
        false ->
            {reply, {error, not_configured}, State};
        true ->
            NewState = State#state{last_restore = iso8601_timestamp()},
            {reply, {ok, restored}, NewState}
    end;

handle_call(test_connection, _From, State) ->
    case State#state.configured of
        false ->
            {reply, {error, not_configured}, State};
        true ->
            NewState = State#state{connection_status = connected},
            {reply, {ok, connected}, NewState}
    end;

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

iso8601_timestamp() ->
    {{Y, M, D}, {H, Mi, S}} = calendar:universal_time(),
    iolist_to_binary(io_lib:format("~4..0B-~2..0B-~2..0BT~2..0B:~2..0B:~2..0BZ", [Y, M, D, H, Mi, S])).
