%%%-------------------------------------------------------------------
%%% @doc Scraper Pool - Manages concurrent web scraping workers
%%%-------------------------------------------------------------------
-module(scraper_pool).
-behaviour(gen_server).

-export([start_link/0, scrape_url/1, process_file/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-define(MAX_CONCURRENT, 10).
-define(TIMEOUT, 30000).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

scrape_url(Url) ->
    gen_server:call(?MODULE, {scrape, Url}, ?TIMEOUT).

process_file(Type, Content) ->
    gen_server:call(?MODULE, {process, Type, Content}, ?TIMEOUT).

init([]) ->
    {ok, #{active => 0, queue => []}}.

handle_call({scrape, Url}, From, #{active := Active} = State) when Active < ?MAX_CONCURRENT ->
    spawn_link(fun() -> do_scrape(Url, From) end),
    {noreply, State#{active := Active + 1}};

handle_call({scrape, Url}, From, #{queue := Queue} = State) ->
    {noreply, State#{queue := Queue ++ [{scrape, Url, From}]}};

handle_call({process, Type, Content}, _From, State) ->
    Result = do_process(Type, Content),
    {reply, Result, State};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({done, _Result}, #{active := Active, queue := []} = State) ->
    {noreply, State#{active := Active - 1}};

handle_info({done, _Result}, #{active := Active, queue := [{scrape, Url, From} | Rest]} = State) ->
    spawn_link(fun() -> do_scrape(Url, From) end),
    {noreply, State#{queue := Rest}};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

do_scrape(Url, From) ->
    Result = case hackney:request(get, Url, [], <<>>, [{timeout, ?TIMEOUT}, {follow_redirect, true}]) of
        {ok, 200, Headers, ClientRef} ->
            {ok, Body} = hackney:body(ClientRef),
            ContentType = proplists:get_value(<<"content-type">>, Headers, <<"text/html">>),
            {ok, #{
                <<"url">> => Url,
                <<"content">> => Body,
                <<"content_type">> => ContentType,
                <<"size">> => byte_size(Body)
            }};
        {ok, Status, _, _} ->
            {error, #{<<"status">> => Status, <<"url">> => Url}};
        {error, Reason} ->
            {error, #{<<"reason">> => list_to_binary(io_lib:format("~p", [Reason])), <<"url">> => Url}}
    end,
    gen_server:reply(From, Result),
    self() ! {done, Result}.

do_process(<<"file">>, Content) ->
    %% Extract text and analyze
    TextContent = extract_text(Content),
    {ok, #{
        <<"processed">> => true,
        <<"text_length">> => byte_size(TextContent),
        <<"content">> => TextContent
    }};
do_process(_, Content) ->
    {ok, #{<<"processed">> => true, <<"content">> => Content}}.

extract_text(Content) when is_binary(Content) ->
    %% Simple HTML tag stripping
    Re = <<"<[^>]*>">>,
    re:replace(Content, Re, <<" ">>, [global, {return, binary}]);
extract_text(Content) ->
    list_to_binary(Content).
