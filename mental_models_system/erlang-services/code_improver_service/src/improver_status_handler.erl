-module(improver_status_handler).
-behaviour(cowboy_handler).

-export([init/2]).

init(Req0, State) ->
    Method = cowboy_req:method(Req0),
    handle_request(Method, Req0, State).

handle_request(<<"GET">>, Req0, State) ->
    case State of
        [] ->
            Status = improver_worker:get_status(),
            Response = #{
                <<"status">> => maps:get(status, Status, idle),
                <<"lm_studio_url">> => maps:get(lm_studio_url, Status, <<>>),
                <<"last_check">> => maps:get(last_check, Status, null),
                <<"improvement_count">> => maps:get(improvement_count, Status, 0),
                <<"pending_count">> => maps:get(pending_count, Status, 0),
                <<"deployed_count">> => maps:get(deployed_count, Status, 0),
                <<"failed_count">> => maps:get(failed_count, Status, 0),
                <<"auto_deploy">> => maps:get(auto_deploy, Status, false),
                <<"design_philosophy">> => <<"Munger Mental Models Aligned">>
            },
            Body = jsx:encode(Response),
            Req = cowboy_req:reply(200,
                #{<<"content-type">> => <<"application/json">>},
                Body,
                Req0),
            {ok, Req, State};
        [list] ->
            Improvements = improver_worker:get_improvements(),
            FormattedImprovements = [format_improvement_summary(I) || I <- Improvements],
            Response = #{
                <<"improvements">> => FormattedImprovements,
                <<"total">> => length(Improvements)
            },
            Body = jsx:encode(Response),
            Req = cowboy_req:reply(200,
                #{<<"content-type">> => <<"application/json">>},
                Body,
                Req0),
            {ok, Req, State};
        [detail] ->
            ImprovementId = cowboy_req:binding(id, Req0),
            case improver_worker:get_improvement(ImprovementId) of
                [Improvement] ->
                    Body = jsx:encode(format_improvement_detail(Improvement)),
                    Req = cowboy_req:reply(200,
                        #{<<"content-type">> => <<"application/json">>},
                        Body,
                        Req0),
                    {ok, Req, State};
                [] ->
                    Req = cowboy_req:reply(404,
                        #{<<"content-type">> => <<"application/json">>},
                        jsx:encode(#{<<"error">> => <<"Improvement not found">>}),
                        Req0),
                    {ok, Req, State}
            end
    end;

handle_request(_, Req0, State) ->
    Req = cowboy_req:reply(405,
        #{<<"content-type">> => <<"application/json">>},
        jsx:encode(#{<<"error">> => <<"Method not allowed">>}),
        Req0),
    {ok, Req, State}.

format_improvement_summary(Improvement) ->
    #{
        <<"id">> => maps:get(id, Improvement, <<>>),
        <<"file_path">> => maps:get(file_path, Improvement, <<>>),
        <<"status">> => maps:get(status, Improvement, pending),
        <<"confidence">> => maps:get(confidence, Improvement, 0.0),
        <<"created_at">> => maps:get(created_at, Improvement, 0)
    }.

format_improvement_detail(Improvement) ->
    #{
        <<"id">> => maps:get(id, Improvement, <<>>),
        <<"file_path">> => maps:get(file_path, Improvement, <<>>),
        <<"status">> => maps:get(status, Improvement, pending),
        <<"confidence">> => maps:get(confidence, Improvement, 0.0),
        <<"created_at">> => maps:get(created_at, Improvement, 0),
        <<"analysis">> => maps:get(analysis, Improvement, <<>>),
        <<"suggested_improvements">> => maps:get(suggested_improvements, Improvement, []),
        <<"risks">> => maps:get(risks, Improvement, []),
        <<"code_smells">> => format_code_smells(maps:get(code_smells, Improvement, []))
    }.

format_code_smells(Smells) ->
    [#{<<"type">> => atom_to_binary(Type, utf8), <<"description">> => Desc} 
     || {Type, Desc} <- Smells].
