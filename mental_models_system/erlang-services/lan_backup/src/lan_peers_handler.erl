-module(lan_peers_handler).
-behaviour(cowboy_handler).

-export([init/2]).

init(Req0, State) ->
    Method = cowboy_req:method(Req0),
    handle_request(Method, Req0, State).

handle_request(<<"GET">>, Req0, State) ->
    Peers = lan_worker:get_peers(),
    Response = jsx:encode(#{success => true, peers => Peers}),
    Req = cowboy_req:reply(200,
        #{<<"content-type">> => <<"application/json">>},
        Response,
        Req0),
    {ok, Req, State};

handle_request(<<"POST">>, Req0, State) ->
    {ok, Body, Req1} = cowboy_req:read_body(Req0),
    Peer = jsx:decode(Body, [return_maps]),
    ok = lan_worker:add_peer(Peer),
    Response = jsx:encode(#{success => true, message => <<"Peer added">>}),
    Req = cowboy_req:reply(200,
        #{<<"content-type">> => <<"application/json">>},
        Response,
        Req1),
    {ok, Req, State};

handle_request(<<"DELETE">>, Req0, State) ->
    {ok, Body, Req1} = cowboy_req:read_body(Req0),
    Data = jsx:decode(Body, [return_maps]),
    PeerId = maps:get(<<"id">>, Data),
    ok = lan_worker:remove_peer(PeerId),
    Response = jsx:encode(#{success => true, message => <<"Peer removed">>}),
    Req = cowboy_req:reply(200,
        #{<<"content-type">> => <<"application/json">>},
        Response,
        Req1),
    {ok, Req, State};

handle_request(_, Req0, State) ->
    Response = jsx:encode(#{error => <<"Method not allowed">>}),
    Req = cowboy_req:reply(405,
        #{<<"content-type">> => <<"application/json">>},
        Response,
        Req0),
    {ok, Req, State}.
