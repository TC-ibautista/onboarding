-module(api_cowboy_handler).
-behaviour(cowboy_handler).

-export([init/2]).

init(Req0, State) ->
    Method = cowboy_req:method(Req0),
    Path = cowboy_req:path(Req0),
    case {Method, Path} of
        {<<"GET">>, <<"/">>} ->
            handle_root(Req0, State);
        {<<"GET">>, <<"/api/v1/items">>} ->
            handle_get_items(Req0, State);
        {<<"GET">>, <<"/api/v1/items/", ItemName/binary>>} ->
            handle_get_item(Req0, State, ItemName);
        {<<"POST">>, <<"/api/v1/items">>} ->
            handle_post_items(Req0, State);
        {<<"PUT">>, <<"/api/v1/items/", ItemName/binary>>} ->
            handle_put_item(Req0, State, ItemName);
        {<<"PATCH">>, <<"/api/v1/items/", ItemName/binary>>} ->
            handle_patch_item_price(Req0, State, ItemName);
        {<<"DELETE">>, <<"/api/v1/items/", ItemName/binary>>} ->
            handle_delete_item(Req0, State, ItemName);
        _ ->
            handle_method_not_allowed(Req0, State)
    end.

handle_root(Req0, State) ->
    Req = cowboy_req:reply(200,
        #{<<"content-type">> => <<"text/plain">>},
        <<"Welcome to the API Cowboy!">>,
        Req0),
    {ok, Req, State}.

handle_get_items(Req0, State) ->
    {ok, Client} = connect_to_redis(),
    {ok, Keys} = eredis:q(Client, ["KEYS", "*"]),
    Items = [{Key, jsx:decode(Value)} || Key <- Keys, {ok, Value} <- [eredis:q(Client, ["GET", Key])]],
    ItemsWithKeys = [#{<<"key">> => Key, <<"value">> => Value} || {Key, Value} <- Items],
    Json = jsx:encode(ItemsWithKeys),

    Req = cowboy_req:reply(200,
        #{<<"content-type">> => <<"application/json">>},
        Json,
        Req0),
    {ok, Req, State}.

handle_get_item(Req0, State, ItemName) ->
    {ok, Client} = connect_to_redis(),
    {ok, Value} = eredis:q(Client, ["GET", ItemName]),
    case Value of
        undefined ->
            Req = cowboy_req:reply(404,
                #{<<"content-type">> => <<"text/plain">>},
                <<"Item not found">>,
                Req0),
            {ok, Req, State};
        _ ->
            Req = cowboy_req:reply(200,
                #{<<"content-type">> => <<"application/json">>},
                Value,
                Req0),
            {ok, Req, State}
    end.

handle_post_items(Req0, State) ->
    {ok, Body, _} = cowboy_req:read_body(Req0),
    ParsedList = jsx:decode(Body),
    ParsedBody = maps:from_list(ParsedList),

    Name = maps:get(<<"name">>, ParsedBody),
    {ok, Client} = connect_to_redis(),
    {ok, <<"OK">>} = eredis:q(Client, ["SET", Name, jsx:encode(ParsedBody)]),

    Result = <<"Item ", Name/binary, " created successfully">>,
    Req = cowboy_req:reply(201, #{<<"content-type">> => <<"application/json">>},
        Result,
        Req0),
    {ok, Req, State}.

handle_put_item(Req0, State, ItemName) ->
    {ok, Body, _} = cowboy_req:read_body(Req0),
    ParsedList = jsx:decode(Body),
    ParsedBody = maps:from_list(ParsedList),

    {ok, Client} = connect_to_redis(),
    {ok, <<"OK">>} = eredis:q(Client, ["SET", ItemName, jsx:encode(ParsedBody)]),
    Result = <<"Item ", ItemName/binary, " updated successfully">>,

    Req = cowboy_req:reply(200, #{<<"content-type">> => <<"application/json">>},
        Result,
        Req0),
    {ok, Req, State}.

handle_patch_item_price(Req0, State, ItemName) ->
    {ok, Body, _} = cowboy_req:read_body(Req0),
    ParsedList = jsx:decode(Body),
    ParsedBody = maps:from_list(ParsedList),

    {ok, Client} = connect_to_redis(),
    {ok, Value} = eredis:q(Client, ["GET", ItemName]),

    CurrentItem = maps:from_list(jsx:decode(Value)),
    NewPrice = maps:get(<<"price">>, ParsedBody, maps:get(<<"price">>, CurrentItem)),
    UpdatedItem = maps:put(<<"price">>, NewPrice, CurrentItem),

    {ok, <<"OK">>} = eredis:q(Client, ["SET", ItemName, jsx:encode(UpdatedItem)]),
    Result = <<"Item ", ItemName/binary, " updated successfully">>,

    Req = cowboy_req:reply(200, #{<<"content-type">> => <<"application/json">>},
        Result,
        Req0),
    {ok, Req, State}.

handle_delete_item(Req0, State, ItemName) ->
    {ok, Client} = connect_to_redis(),
    {ok, DeletedCount} = eredis:q(Client, ["DEL", ItemName]),
    Result = case DeletedCount of
        <<"0">> -> <<"Item ", ItemName/binary, " not found">>;
        <<"1">> -> <<"Item ", ItemName/binary, " deleted successfully">>;
        _ -> <<"Unexpected result: ", DeletedCount/binary>>
    end,
    
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
    case eredis:start_link([{host, RedisHost}, {port, list_to_integer(RedisPort)}]) of
        {ok, Client} ->
            {ok, Client};
        {error, Reason} ->
            io:format("Failed to start Redis client: ~p~n", [Reason]),
            {error, Reason}
    end.