-module(lm_fix_error_handler).
-behaviour(cowboy_handler).
-export([init/2]).

init(Req0, State) ->
    case cowboy_req:method(Req0) of
        <<"POST">> ->
            {ok, Body, Req1} = cowboy_req:read_body(Req0),
            handle_fix(Body, Req1, State);
        _ ->
            Req = cowboy_req:reply(405, #{<<"content-type">> => <<"application/json">>},
                jsx:encode(#{<<"error">> => <<"Method not allowed">>}), Req0),
            {ok, Req, State}
    end.

handle_fix(Body, Req0, State) ->
    try jsx:decode(Body, [return_maps]) of
        Params ->
            FilePath = maps:get(<<"file_path">>, Params, <<>>),
            ErrorOutput = maps:get(<<"error_output">>, Params, <<>>),
            
            Result = error_fixer:fix_compile_error(
                binary_to_list(FilePath), 
                binary_to_list(ErrorOutput)
            ),
            
            Response = case Result of
                {ok, FixResult} ->
                    jsx:encode(#{<<"success">> => true, <<"result">> => FixResult});
                {error, Reason} ->
                    jsx:encode(#{<<"success">> => false, 
                                 <<"error">> => list_to_binary(io_lib:format("~p", [Reason]))})
            end,
            
            Req = cowboy_req:reply(200, #{<<"content-type">> => <<"application/json">>},
                Response, Req0),
            {ok, Req, State}
    catch
        _:_ ->
            Req = cowboy_req:reply(400, #{<<"content-type">> => <<"application/json">>},
                jsx:encode(#{<<"error">> => <<"Invalid JSON">>}), Req0),
            {ok, Req, State}
    end.
