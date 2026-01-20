-module(lm_studio_client).

-export([
    chat/3,
    chat/4,
    health_check/1,
    generate_improvement/3
]).

health_check(BaseUrl) ->
    Url = BaseUrl ++ "/v1/models",
    case httpc:request(get, {Url, []}, [{timeout, 5000}], []) of
        {ok, {{_, 200, _}, _, _}} ->
            {ok, healthy};
        {ok, {{_, Status, _}, _, _}} ->
            {error, {http_error, Status}};
        {error, Reason} ->
            {error, Reason}
    end.

chat(BaseUrl, Messages, Options) ->
    chat(BaseUrl, Messages, Options, 60000).

chat(BaseUrl, Messages, Options, Timeout) ->
    Url = BaseUrl ++ "/v1/chat/completions",
    
    Model = maps:get(model, Options, <<"local-model">>),
    MaxTokens = maps:get(max_tokens, Options, 4000),
    Temperature = maps:get(temperature, Options, 0.3),
    
    RequestBody = jsx:encode(#{
        <<"model">> => Model,
        <<"messages">> => format_messages(Messages),
        <<"max_tokens">> => MaxTokens,
        <<"temperature">> => Temperature
    }),
    
    Headers = [{"Content-Type", "application/json"}],
    
    case httpc:request(post, {Url, Headers, "application/json", RequestBody}, 
                       [{timeout, Timeout}], [{body_format, binary}]) of
        {ok, {{_, 200, _}, _, ResponseBody}} ->
            try
                Response = jsx:decode(ResponseBody, [return_maps]),
                Choices = maps:get(<<"choices">>, Response, []),
                case Choices of
                    [FirstChoice | _] ->
                        Message = maps:get(<<"message">>, FirstChoice, #{}),
                        Content = maps:get(<<"content">>, Message, <<>>),
                        {ok, Content};
                    [] ->
                        {error, no_choices}
                end
            catch
                _:_ ->
                    {error, json_parse_error}
            end;
        {ok, {{_, Status, _}, _, Body}} ->
            {error, {http_error, Status, Body}};
        {error, Reason} ->
            {error, Reason}
    end.

format_messages(Messages) ->
    [format_message(M) || M <- Messages].

format_message({Role, Content}) when is_atom(Role) ->
    #{<<"role">> => atom_to_binary(Role, utf8),
      <<"content">> => ensure_binary(Content)};
format_message(#{role := Role, content := Content}) ->
    #{<<"role">> => ensure_binary(Role),
      <<"content">> => ensure_binary(Content)};
format_message(Map) when is_map(Map) ->
    Map.

ensure_binary(B) when is_binary(B) -> B;
ensure_binary(L) when is_list(L) -> list_to_binary(L);
ensure_binary(A) when is_atom(A) -> atom_to_binary(A, utf8).

generate_improvement(BaseUrl, SystemPrompt, UserPrompt) ->
    Messages = [
        {system, SystemPrompt},
        {user, UserPrompt}
    ],
    Options = #{
        max_tokens => 4000,
        temperature => 0.3
    },
    case chat(BaseUrl, Messages, Options, 120000) of
        {ok, Content} ->
            parse_improvement_response(Content);
        {error, Reason} ->
            {error, Reason}
    end.

parse_improvement_response(Content) ->
    try
        JsonStart = find_json_start(Content),
        JsonEnd = find_json_end(Content, JsonStart),
        JsonStr = binary:part(Content, JsonStart, JsonEnd - JsonStart + 1),
        Parsed = jsx:decode(JsonStr, [return_maps]),
        {ok, Parsed}
    catch
        _:_ ->
            {ok, #{<<"raw_response">> => Content, <<"parse_error">> => true}}
    end.

find_json_start(Binary) ->
    find_json_start(Binary, 0).

find_json_start(Binary, Pos) when Pos < byte_size(Binary) ->
    case binary:at(Binary, Pos) of
        ${ -> Pos;
        $[ -> Pos;
        _ -> find_json_start(Binary, Pos + 1)
    end;
find_json_start(_, Pos) ->
    Pos.

find_json_end(Binary, Start) ->
    find_json_end(Binary, Start, byte_size(Binary) - 1).

find_json_end(Binary, Start, Pos) when Pos > Start ->
    case binary:at(Binary, Pos) of
        $} -> Pos;
        $] -> Pos;
        _ -> find_json_end(Binary, Start, Pos - 1)
    end;
find_json_end(_, _, Pos) ->
    Pos.
