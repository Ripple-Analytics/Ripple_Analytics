%%%-------------------------------------------------------------------
%%% @doc File Utilities - Read, write, format helpers
%%% @end
%%%-------------------------------------------------------------------
-module(file_utils).

-export([read/1, write/2, delete/1, ensure_dir/1]).
-export([fmt/1, format_time/1, truncate/2]).

%% @doc Read file, return undefined if missing/empty
read(Path) ->
    case file:read_file(Path) of
        {ok, Bin} -> 
            case string:trim(binary_to_list(Bin)) of
                "" -> undefined;
                S -> list_to_binary(S)
            end;
        {error, _} -> undefined
    end.

%% @doc Write content to file
write(Path, Content) when is_binary(Content) ->
    file:write_file(Path, Content);
write(Path, Content) when is_list(Content) ->
    file:write_file(Path, Content).

%% @doc Delete file
delete(Path) ->
    file:delete(Path).

%% @doc Ensure directory exists
ensure_dir(Path) ->
    filelib:ensure_dir(Path ++ "/").

%% @doc Format value for logging
fmt(undefined) -> "undefined";
fmt(Bin) when is_binary(Bin) -> binary_to_list(Bin);
fmt(Other) -> io_lib:format("~p", [Other]).

%% @doc Format timestamp for JSON
format_time(undefined) -> null;
format_time({MegaSecs, Secs, _}) ->
    Seconds = MegaSecs * 1000000 + Secs,
    list_to_binary(calendar:system_time_to_rfc3339(Seconds, [{unit, second}])).

%% @doc Truncate string
truncate(Str, Max) when is_list(Str), length(Str) > Max ->
    string:sub_string(Str, 1, Max);
truncate(Str, _) when is_list(Str) -> Str;
truncate(_, _) -> "".
