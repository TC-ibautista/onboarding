-module(db_operations).
-export([get_item/1, get_all_items/0, create_item/2, update_item/2, delete_item/1, patch_item_price/2]).

get_item(ItemName) ->
    case redis_connection:connect() of
        {ok, Client} ->
            eredis:q(Client, ["GET", ItemName]);
        {error, Reason} ->
            {error, Reason}
    end.

get_all_items() ->
    case redis_connection:connect() of
        {ok, Client} ->
            {ok, Keys} = eredis:q(Client, ["KEYS", "*"]),
            Items = [{Key, jsx:decode(Value)} || Key <- Keys, {ok, Value} <- [eredis:q(Client, ["GET", Key])]],
            {ok, Items};
        {error, Reason} ->
            {error, Reason}
    end.

create_item(ItemName, Item) ->
    case redis_connection:connect() of
        {ok, Client} ->
            eredis:q(Client, ["SET", ItemName, jsx:encode(Item)]);
        {error, Reason} ->
            {error, Reason}
    end.

update_item(ItemName, Item) ->
    case redis_connection:connect() of
        {ok, Client} ->
            eredis:q(Client, ["SET", ItemName, jsx:encode(Item)]);
        {error, Reason} ->
            {error, Reason}
    end.

delete_item(ItemName) ->
    case redis_connection:connect() of
        {ok, Client} ->
            eredis:q(Client, ["DEL", ItemName]);
        {error, Reason} ->
            {error, Reason}
    end.

patch_item_price(ItemName, NewPrice) ->
    case redis_connection:connect() of
        {ok, Client} ->
            case eredis:q(Client, ["GET", ItemName]) of
                {ok, Value} when Value =/= undefined ->
                    CurrentItem = maps:from_list(jsx:decode(Value)),
                    UpdatedItem = maps:put(<<"price">>, NewPrice, CurrentItem),
                    eredis:q(Client, ["SET", ItemName, jsx:encode(UpdatedItem)]);
                {ok, undefined} ->
                    {error, <<"Item not found">>};
                {error, Reason} ->
                    {error, Reason}
            end;
        {error, Reason} ->
            {error, Reason}
    end.