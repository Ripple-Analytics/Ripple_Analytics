-module(lm_studio_client).
-behaviour(gen_server).

-export([start_link/0, generate/2, generate/3, check_connection/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-record(state, {
    base_url :: string(),
    connected :: boolean(),
    last_check :: integer()
}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

generate(Prompt, SystemPrompt) ->
    generate(Prompt, SystemPrompt, #{}).

generate(Prompt, SystemPrompt, Options) ->
    gen_server:call(?MODULE, {generate, Prompt, SystemPrompt, Options}, 120000).

check_connection() ->
    gen_server:call(?MODULE, check_connection, 10000).

init([]) ->
    BaseUrl = application:get_env(ai_code_improver, lm_studio_url, "http://host.docker.internal:1234"),
    {ok, #state{
        base_url = BaseUrl,
        connected = false,
        last_check = 0
    }}.

handle_call({generate, Prompt, SystemPrompt, Options}, _From, State) ->
    Temperature = maps:get(temperature, Options, 0.7),
    MaxTokens = maps:get(max_tokens, Options, 4096),
    
    RequestBody = jsx:encode(#{
        <<"model">> => <<"local-model">>,
        <<"messages">> => [
            #{<<"role">> => <<"system">>, <<"content">> => list_to_binary(SystemPrompt)},
            #{<<"role">> => <<"user">>, <<"content">> => list_to_binary(Prompt)}
        ],
        <<"temperature">> => Temperature,
        <<"max_tokens">> => MaxTokens,
        <<"stream">> => false
    }),
    
    Url = State#state.base_url ++ "/v1/chat/completions",
    
    case hackney:request(post, list_to_binary(Url), 
                         [{<<"Content-Type">>, <<"application/json">>}],
                         RequestBody, 
                         [{timeout, 120000}, {recv_timeout, 120000}]) of
        {ok, 200, _Headers, ClientRef} ->
            {ok, Body} = hackney:body(ClientRef),
            Response = jsx:decode(Body, [return_maps]),
            Choices = maps:get(<<"choices">>, Response, []),
            case Choices of
                [FirstChoice | _] ->
                    Message = maps:get(<<"message">>, FirstChoice, #{}),
                    Content = maps:get(<<"content">>, Message, <<>>),
                    {reply, {ok, Content}, State#state{connected = true}};
                [] ->
                    {reply, {error, no_response}, State}
            end;
        {ok, StatusCode, _Headers, ClientRef} ->
            {ok, Body} = hackney:body(ClientRef),
            {reply, {error, {http_error, StatusCode, Body}}, State};
        {error, Reason} ->
            {reply, {error, Reason}, State#state{connected = false}}
    end;

handle_call(check_connection, _From, State) ->
    Url = State#state.base_url ++ "/v1/models",
    case hackney:request(get, list_to_binary(Url), [], <<>>, [{timeout, 5000}]) of
        {ok, 200, _Headers, ClientRef} ->
            hackney:body(ClientRef),
            {reply, {ok, connected}, State#state{connected = true, last_check = erlang:system_time(second)}};
        {ok, StatusCode, _Headers, _ClientRef} ->
            {reply, {error, {http_error, StatusCode}}, State#state{connected = false}};
        {error, Reason} ->
            {reply, {error, Reason}, State#state{connected = false}}
    end.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.
