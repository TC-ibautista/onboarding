-module(db_operations).
-export([get_item/1, get_all_items/0, create_item/2, update_item/2, delete_item/1]).

get_item(ItemName) ->
    Client = redis_pool:checkout(),
    Result = eredis:q(Client, ["GET", ItemName]),
    redis_pool:checkin(Client),
    Result.

get_all_items() ->
    Client = redis_pool:checkout(),
    {ok, Keys} = eredis:q(Client, ["KEYS", "*"]),
    Items = [{Key, jsx:decode(Value)} || Key <- Keys, {ok, Value} <- [eredis:q(Client, ["GET", Key])]],
    redis_pool:checkin(Client),
    {ok, Items}.

create_item(ItemName, Item) ->
    Client = redis_pool:checkout(),
    Result = eredis:q(Client, ["SET", ItemName, jsx:encode(Item)]),
    redis_pool:checkin(Client),
    Result.

update_item(ItemName, Item) ->
    Client = redis_pool:checkout(),
    Result = eredis:q(Client, ["SET", ItemName, jsx:encode(Item)]),
    redis_pool:checkin(Client),
    Result.

delete_item(ItemName) ->
    Client = redis_pool:checkout(),
    Result = eredis:q(Client, ["DEL", ItemName]),
    redis_pool:checkin(Client),
    Result.
