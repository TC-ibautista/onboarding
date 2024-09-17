-module(api_cowboy_handler).
-behaviour(cowboy_handler).

-export([init/2]).

init(Req0, State) ->
    Method = cowboy_req:method(Req0),
    Path = cowboy_req:path(Req0),
    case {Method, Path} of
        {<<"GET">>, <<"/">>} ->
            handle_cowboy(Req0, State);
        {<<"GET">>, <<"/api/v1/items">>} ->
            handle_get_items(Req0, State);
        {<<"POST">>, <<"/api/v1/items">>} ->
            handle_post_items(Req0, State);
        {<<"PUT">>, <<"/api/v1/items/", _/binary>>} ->
            handle_put_item(Req0, State);
        {<<"PATCH">>, <<"/api/v1/items/", _/binary>>} ->
            handle_patch_item(Req0, State);
        {<<"DELETE">>, <<"/api/v1/items/", _/binary>>} ->
            handle_delete_item(Req0, State);
        _ ->
            handle_method_not_allowed(Req0, State)
    end.

handle_cowboy(Req0, State) ->
    Req = cowboy_req:reply(200,
        #{<<"content-type">> => <<"text/plain">>},
        <<"Hello, Cowboy!">>,
        Req0),
    {ok, Req, State}.

handle_get_items(Req0, State) ->
    Items = [
        #{id => 1, name => <<"Item 1">>, price => 100},
        #{id => 2, name => <<"Item 2">>, price => 200},
        #{id => 3, name => <<"Item 3">>, price => 300}
    ],
    Json = jsx:encode(Items),
    Req = cowboy_req:reply(200,
        #{<<"content-type">> => <<"application/json">>},
        Json,
        Req0),
    {ok, Req, State}.

handle_post_items(Req0, State) ->
    {ok, Body, _} = cowboy_req:read_body(Req0),
    ParsedList = jsx:decode(Body),
    ParsedBody = maps:from_list(ParsedList),

    Name = maps:get(<<"name">>, ParsedBody),
    Result = <<"Item ", Name/binary, " created successfully">>,

    %% Connect to Redis and store the item
    % Client = connect_to_redis(),
    % ok = eredis:q(Client, ["SET", Name, Result]),

    Req = cowboy_req:reply(201, #{<<"content-type">> => <<"application/json">>},
        Result,
        Req0),
    {ok, Req, State}.

handle_put_item(Req0, State) ->
    Id = cowboy_req:binding(id, Req0),
    {ok, Body, _} = cowboy_req:read_body(Req0),
    ParsedList = jsx:decode(Body),
    ParsedBody = maps:from_list(ParsedList),

    NewName = maps:get(<<"name">>, ParsedBody),
    Result = <<"Item with ID:",Id/binary," - ",NewName/binary," updated successfully">>,

    Req = cowboy_req:reply(200, #{<<"content-type">> => <<"application/json">>},
        Result,
        Req0),
    {ok, Req, State}.

handle_patch_item(Req0, State) ->
    Id = cowboy_req:binding(id, Req0),
    {ok, Body, _} = cowboy_req:read_body(Req0),
    ParsedList = jsx:decode(Body),
    ParsedBody = maps:from_list(ParsedList),

    NewPrice = maps:get(<<"price">>, ParsedBody),
    NewPriceBinary = integer_to_binary(NewPrice),
    Result = <<"Item with ID:",
        Id/binary,
        " - New price: ",
        NewPriceBinary/binary,
        " patched successfully">>,

    Req = cowboy_req:reply(200, #{<<"content-type">> => <<"application/json">>},
        Result,
        Req0),
    {ok, Req, State}.

handle_delete_item(Req0, State) ->
    Id = cowboy_req:binding(id, Req0),
    Result = <<"Item with ID:", Id/binary, " deleted successfully">>,
    Req = cowboy_req:reply(200, #{<<"content-type">> => <<"application/json">>},
        Result,
        Req0),
    {ok, Req, State}.

handle_method_not_allowed(Req0, State) ->
    Req = cowboy_req:reply(405,
        #{<<"content-type">> => <<"text/plain">>},
        <<"Method Not Allowed">>,
        Req0),
    {ok, Req, State}.

connect_to_redis() ->
    RedisHost = os:getenv("REDIS_HOST"),
    RedisPort = os:getenv("REDIS_PORT"),
    {ok, Client} = eredis:start_link([{host, RedisHost}, {port, RedisPort}]),
    Client.