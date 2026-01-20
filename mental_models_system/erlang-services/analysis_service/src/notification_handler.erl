%%%-------------------------------------------------------------------
%%% @doc Notification HTTP Handler - Server-Sent Events
%%% 
%%% Provides SSE endpoint for real-time notifications.
%%% 
%%% Endpoints:
%%% GET /api/analysis/notifications - SSE stream for real-time notifications
%%% GET /api/analysis/notifications/recent - Get recent notifications
%%% @end
%%%-------------------------------------------------------------------
-module(notification_handler).

-export([init/2, info/3, terminate/3]).

init(Req0, State) ->
    case cowboy_req:method(Req0) of
        <<"GET">> ->
            Path = cowboy_req:path(Req0),
            case binary:match(Path, <<"/recent">>) of
                nomatch ->
                    init_sse(Req0, State);
                _ ->
                    handle_recent(Req0, State)
            end;
        _ ->
            Req = cowboy_req:reply(405, #{
                <<"content-type">> => <<"application/json">>
            }, <<"{\"error\":\"Method not allowed\"}">>, Req0),
            {ok, Req, State}
    end.

init_sse(Req0, State) ->
    Req = cowboy_req:stream_reply(200, #{
        <<"content-type">> => <<"text/event-stream">>,
        <<"cache-control">> => <<"no-cache">>,
        <<"connection">> => <<"keep-alive">>,
        <<"access-control-allow-origin">> => <<"*">>
    }, Req0),
    
    notification_service:subscribe(self()),
    
    cowboy_req:stream_body(<<"event: connected\ndata: {\"status\":\"connected\"}\n\n">>, nofin, Req),
    
    {cowboy_loop, Req, State, hibernate}.

handle_recent(Req0, State) ->
    {ok, Recent} = notification_service:get_recent(),
    Body = jsx:encode(#{<<"notifications">> => Recent}),
    Req = cowboy_req:reply(200, #{
        <<"content-type">> => <<"application/json">>,
        <<"access-control-allow-origin">> => <<"*">>
    }, Body, Req0),
    {ok, Req, State}.

info({notification, Notification}, Req, State) ->
    Data = jsx:encode(Notification),
    Type = maps:get(<<"type">>, Notification, <<"notification">>),
    Message = iolist_to_binary([
        <<"event: ">>, Type, <<"\n">>,
        <<"data: ">>, Data, <<"\n\n">>
    ]),
    cowboy_req:stream_body(Message, nofin, Req),
    {ok, Req, State, hibernate};

info(_Info, Req, State) ->
    {ok, Req, State, hibernate}.

terminate(_Reason, _Req, _State) ->
    notification_service:unsubscribe(self()),
    ok.
