%%%-------------------------------------------------------------------
%%% @doc update_handler Helper Module - Part 2
%%% @end
%%%-------------------------------------------------------------------
-module(update_handler_part2).

-export([handle_restart_updater/2, default_status/0, default_config/0, mask_sensitive/1, mask_token/1, mask_token/1, mask_token/1, mask_url/1, mask_url/1, mask_url/1, write_env_file/1, cors_headers/0]).

handle_restart_updater(Req0, State) ->
    Result = os:cmd("docker restart mental-models-auto-updater 2>&1"),
    
    Response = #{
        <<"success">> => true,
        <<"message">> => <<"Auto-updater restarted">>,
        <<"details">> => list_to_binary(Result)
    },
    
    Req = cowboy_req:reply(200, cors_headers(),
        jsx:encode(Response), Req0),
    {ok, Req, State}.

%% Internal functions

default_status() ->
    #{
        <<"status">> => <<"unknown">>,
        <<"message">> => <<"Status not available">>,
        <<"github_available">> => false,
        <<"last_check">> => <<"never">>,
        <<"update_source">> => <<"unknown">>
    }.

default_config() ->
    #{
        <<"github_token_configured">> => false,
        <<"gdrive_url_configured">> => false,
        <<"check_interval">> => 300,
        <<"auto_update_enabled">> => true
    }.

mask_sensitive(Config) ->
    Token = maps:get(<<"github_token">>, Config, <<>>),
    GDrive = maps:get(<<"gdrive_url">>, Config, <<>>),
    
    Config#{
        <<"github_token_configured">> => byte_size(Token) > 0,
        <<"gdrive_url_configured">> => byte_size(GDrive) > 0,
        <<"github_token">> => mask_token(Token),
        <<"gdrive_url">> => mask_url(GDrive)
    }.

mask_token(<<>>) -> <<>>;
mask_token(Token) when byte_size(Token) > 8 ->
    Prefix = binary:part(Token, 0, 4),
    Suffix = binary:part(Token, byte_size(Token) - 4, 4),
    <<Prefix/binary, "...", Suffix/binary>>;
mask_token(_) -> <<"****">>.

mask_url(<<>>) -> <<>>;
mask_url(Url) when byte_size(Url) > 20 ->
    Prefix = binary:part(Url, 0, 15),
    <<Prefix/binary, "...">>;
mask_url(Url) -> Url.

write_env_file(Config) ->
    EnvPath = "/data/.env",
    
    Lines = [
        case maps:get(<<"github_token">>, Config, <<>>) of
            <<>> -> "";
            Token -> io_lib:format("GITHUB_TOKEN=~s~n", [Token])
        end,
        case maps:get(<<"gdrive_url">>, Config, <<>>) of
            <<>> -> "";
            Url -> io_lib:format("GDRIVE_BACKUP_URL=~s~n", [Url])
        end,
        case maps:get(<<"check_interval">>, Config, 300) of
            Interval -> io_lib:format("UPDATE_CHECK_INTERVAL=~p~n", [Interval])
        end
    ],
    
    Content = iolist_to_binary(Lines),
    file:write_file(EnvPath, Content).

cors_headers() ->
    #{<<"content-type">> => <<"application/json">>,
      <<"access-control-allow-origin">> => <<"*">>,
      <<"access-control-allow-methods">> => <<"GET, POST, OPTIONS">>,
      <<"access-control-allow-headers">> => <<"Content-Type">>}.

