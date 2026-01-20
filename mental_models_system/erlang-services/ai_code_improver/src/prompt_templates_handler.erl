-module(prompt_templates_handler).
-behaviour(cowboy_handler).

-export([init/2]).

init(Req0, State) ->
    Templates = #{
        <<"improvement">> => #{
            <<"description">> => <<"Generate code improvements based on analysis">>,
            <<"example_usage">> => <<"POST /api/improver/suggest with {\"file\": \"/path/to/file.erl\"}">>
        },
        <<"analysis">> => #{
            <<"description">> => <<"Analyze code for issues and opportunities">>,
            <<"example_usage">> => <<"POST /api/improver/analyze with {\"path\": \"/path/to/dir\"}">>
        },
        <<"validation">> => #{
            <<"description">> => <<"Validate generated code before deployment">>,
            <<"example_usage">> => <<"POST /api/improver/validate with {\"code\": \"...erlang code...\"}">>
        },
        <<"system_prompt_structure">> => #{
            <<"sections">> => [
                <<"Architecture Principles">>,
                <<"Code Style Guidelines">>,
                <<"Service Patterns">>,
                <<"Mental Models Domain">>
            ],
            <<"key_behaviors">> => [
                <<"gen_server">>,
                <<"cowboy_handler">>,
                <<"supervisor">>,
                <<"application">>
            ]
        }
    },
    
    Response = jsx:encode(#{
        <<"templates">> => Templates,
        <<"note">> => <<"These templates are used by the AI code improver to generate contextually appropriate code improvements">>
    }),
    Req = cowboy_req:reply(200,
        #{<<"content-type">> => <<"application/json">>},
        Response,
        Req0),
    {ok, Req, State}.
