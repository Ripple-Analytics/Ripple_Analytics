-module(design_philosophy_handler).
-behaviour(cowboy_handler).

-export([init/2]).

init(Req0, State) ->
    SystemPrompt = design_philosophy:get_system_prompt(),
    CodeStyleGuidelines = design_philosophy:get_code_style_guidelines(),
    ArchitecturePrinciples = design_philosophy:get_architecture_principles(),
    
    Response = jsx:encode(#{
        <<"system_prompt">> => list_to_binary(SystemPrompt),
        <<"code_style_guidelines">> => format_guidelines(CodeStyleGuidelines),
        <<"architecture_principles">> => format_guidelines(ArchitecturePrinciples)
    }),
    Req = cowboy_req:reply(200,
        #{<<"content-type">> => <<"application/json">>},
        Response,
        Req0),
    {ok, Req, State}.

format_guidelines(Map) when is_map(Map) ->
    maps:fold(fun(K, V, Acc) ->
        Acc#{atom_to_binary(K, utf8) => format_value(V)}
    end, #{}, Map).

format_value(List) when is_list(List) ->
    [list_to_binary(Item) || Item <- List, is_list(Item)];
format_value(Other) ->
    Other.
