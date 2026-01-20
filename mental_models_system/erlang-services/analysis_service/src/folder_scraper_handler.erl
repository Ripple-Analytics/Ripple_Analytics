%%%-------------------------------------------------------------------
%%% @doc Folder Scraper Handler
%%% 
%%% Handles folder scraping and batch analysis requests.
%%% @end
%%%-------------------------------------------------------------------
-module(folder_scraper_handler).
-behaviour(cowboy_handler).

-export([init/2]).

%%--------------------------------------------------------------------
%% @doc Handle folder scraper requests
%% @end
%%--------------------------------------------------------------------
init(Req0, State) ->
    Method = cowboy_req:method(Req0),
    
    case Method of
        <<"POST">> ->
            {ok, Body, Req1} = cowboy_req:read_body(Req0),
            handle_scrape(Body, Req1, State);
        <<"GET">> ->
            handle_info(Req0, State);
        <<"OPTIONS">> ->
            Req = cowboy_req:reply(200, cors_headers(), <<>>, Req0),
            {ok, Req, State};
        _ ->
            Req = cowboy_req:reply(405, cors_headers(), 
                jsx:encode(#{<<"error">> => <<"Method not allowed">>}), Req0),
            {ok, Req, State}
    end.

handle_info(Req0, State) ->
    HostPath = os:getenv("HOST_PATH", ""),
    Info = #{
        <<"success">> => true,
        <<"host_path">> => list_to_binary(HostPath),
        <<"supported_extensions">> => [list_to_binary(E) || E <- folder_scraper:get_supported_extensions()],
        <<"max_file_size">> => <<"1MB">>,
        <<"usage">> => #{
            <<"scan">> => <<"POST with {\"action\": \"scan\", \"folder\": \"path\"}">>,
            <<"analyze">> => <<"POST with {\"action\": \"analyze\", \"folder\": \"path\"}">>,
            <<"analyze_file">> => <<"POST with {\"action\": \"analyze_file\", \"file\": \"path\"}">>
        }
    },
    Req = cowboy_req:reply(200, cors_headers(), jsx:encode(Info), Req0),
    {ok, Req, State}.

handle_scrape(Body, Req0, State) ->
    try jsx:decode(Body, [return_maps]) of
        Params ->
            Action = maps:get(<<"action">>, Params, <<"analyze">>),
            Result = execute_action(Action, Params),
            Req = cowboy_req:reply(200, cors_headers(), jsx:encode(Result), Req0),
            {ok, Req, State}
    catch
        _:_ ->
            Req = cowboy_req:reply(400, cors_headers(),
                jsx:encode(#{<<"error">> => <<"Invalid JSON">>}), Req0),
            {ok, Req, State}
    end.

execute_action(<<"scan">>, Params) ->
    Folder = get_folder_path(Params),
    Options = build_options(Params),
    folder_scraper:scan_folder(Folder, Options);

execute_action(<<"analyze">>, Params) ->
    Folder = get_folder_path(Params),
    Options = build_options(Params),
    folder_scraper:scan_and_analyze(Folder, Options);

execute_action(<<"analyze_file">>, Params) ->
    File = maps:get(<<"file">>, Params, <<>>),
    Options = build_options(Params),
    case File of
        <<>> -> #{<<"error">> => <<"No file path provided">>};
        _ -> folder_scraper:analyze_file(File, Options)
    end;

execute_action(_, _) ->
    #{<<"error">> => <<"Unknown action. Use: scan, analyze, or analyze_file">>}.

get_folder_path(Params) ->
    case maps:get(<<"folder">>, Params, <<>>) of
        <<>> ->
            HostPath = os:getenv("HOST_PATH", "/data"),
            list_to_binary(HostPath);
        Folder ->
            Folder
    end.

build_options(Params) ->
    Recursive = maps:get(<<"recursive">>, Params, false),
    AnalysisType = case maps:get(<<"analysis_type">>, Params, <<"full">>) of
        <<"full">> -> full;
        <<"models">> -> models;
        <<"patterns">> -> patterns;
        <<"quick">> -> quick;
        _ -> full
    end,
    #{recursive => Recursive, analysis_type => AnalysisType}.

cors_headers() ->
    #{<<"content-type">> => <<"application/json">>,
      <<"access-control-allow-origin">> => <<"*">>,
      <<"access-control-allow-methods">> => <<"GET, POST, OPTIONS">>,
      <<"access-control-allow-headers">> => <<"Content-Type">>}.
