%%%-------------------------------------------------------------------
%%% @doc Debug Logger Application - Starts before all other services
%%% Captures all errors and pushes to GitHub for remote debugging
%%% @end
%%%-------------------------------------------------------------------
-module(debug_logger_app).
-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    io:format("[DEBUG_LOGGER] ========================================~n"),
    io:format("[DEBUG_LOGGER] Debug Logger Service Starting~n"),
    io:format("[DEBUG_LOGGER] ========================================~n"),
    debug_logger_sup:start_link().

stop(_State) ->
    ok.
