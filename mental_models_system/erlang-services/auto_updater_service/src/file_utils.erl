%%%-------------------------------------------------------------------
%%% @doc File Utilities - Read, write, format helpers
%%% @end
%%%-------------------------------------------------------------------
-module(file_utils).

-export([read/1, write/2, delete/1, ensure_dir/1]).
-export([fmt/1, format_time/1, truncate/2]).

%% @doc Read file, return undefined if missing/empty
read(Path) ->
    try
        case file:read_file(Path) of
            {ok, Bin} -> 
                case string:trim(binary_to_list(Bin)) of
                    "" -> undefined;
                    S -> list_to_binary(S)
                end;
            {error, _} -> undefined
        end
    catch
        _:_ -> undefined
    end.

%% @doc Write content to file
write(Path, Content) when is_binary(Content) ->
    ensure_parent_dir(Path),
    file:write_file(Path, Content);
write(Path, Content) when is_list(Content) ->
    ensure_parent_dir(Path),
    file:write_file(Path, Content);
write(_, _) -> {error, invalid_content}.

%% @doc Delete file
delete(Path) ->
    file:delete(Path).

%% @doc Ensure directory exists
ensure_dir(Path) ->
    filelib:ensure_dir(Path ++ "/").

%% Internal: ensure parent directory exists
ensure_parent_dir(Path) ->
    filelib:ensure_dir(Path).

%% @doc Format value for logging
fmt(undefined) -> "undefined";
fmt(Bin) when is_binary(Bin) -> binary_to_list(Bin);
fmt(List) when is_list(List) -> List;
fmt(Atom) when is_atom(Atom) -> atom_to_list(Atom);
fmt(Other) -> io_lib:format("~p", [Other]).

%% @doc Format timestamp for JSON
format_time(undefined) -> null;
format_time({MegaSecs, Secs, _}) ->
    try
        Seconds = MegaSecs * 1000000 + Secs,
        list_to_binary(calendar:system_time_to_rfc3339(Seconds, [{unit, second}]))
    catch
        _:_ -> null
    end;
format_time(_) -> null.

%% @doc Truncate string
truncate(Str, Max) when is_list(Str) ->
    case length(Str) > Max of
        true -> string:slice(Str, 0, Max) ++ "...";
        false -> Str
    end;
truncate(Bin, Max) when is_binary(Bin) ->
    truncate(binary_to_list(Bin), Max);
truncate(_, _) -> "".
