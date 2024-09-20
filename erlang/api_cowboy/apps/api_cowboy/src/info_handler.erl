-module(info_handler).
-export([get_item/1, get_all_items/0, create_item/1, update_item/2, delete_item/1, patch_item_price/2]).

get_item(ItemName) ->
    case db_operations:get_item(ItemName) of
        {ok, Value} when Value =/= undefined ->
            {ok, Value};
        {ok, undefined} ->
            {error, <<"Item not found">>};
        {error, Reason} ->
            {error, Reason}
    end.

get_all_items() ->
    case db_operations:get_all_items() of
        {ok, Items} ->
            ItemsWithKeys = [#{<<"key">> => Key, <<"value">> => Value} || {Key, Value} <- Items],
            Json = jsx:encode(ItemsWithKeys),
            {ok, Json};
        {error, Reason} ->
            {error, Reason}
    end.

create_item(Item) ->
    ParsedItem = maps:from_list(jsx:decode(Item)),
    Name = maps:get(<<"name">>, ParsedItem),
    case db_operations:create_item(Name, ParsedItem) of
        {ok, <<"OK">>} ->
            {ok, <<"Item created successfully">>};
        {error, Reason} ->
            {error, Reason}
    end.

update_item(ItemName, Item) ->
    ParsedItem = maps:from_list(jsx:decode(Item)),
    case db_operations:update_item(ItemName, ParsedItem) of
        {ok, <<"OK">>} ->
            {ok, <<"Item updated successfully">>};
        {error, Reason} ->
            {error, Reason}
    end.

delete_item(ItemName) ->
    case db_operations:delete_item(ItemName) of
        {ok, DeletedCount} ->
            case DeletedCount of
                <<"0">> -> {ok, <<"Item not found">>};
                <<"1">> -> {ok, <<"Item deleted successfully">>};
                _ -> {ok, <<"Unexpected result: ", DeletedCount/binary>>}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

patch_item_price(ItemName, Item) ->
    ParsedItem = maps:from_list(jsx:decode(Item)),
    NewPrice = maps:get(<<"price">>, ParsedItem),
    case get_item(ItemName) of 
        {ok, CurrentItem} ->
            ParsedCurrentItem = maps:from_list(jsx:decode(CurrentItem)),
            UpdatedItem = maps:put(<<"price">>, NewPrice, ParsedCurrentItem),
            case db_operations:update_item(ItemName, UpdatedItem) of
                {ok, <<"OK">>} ->
                    {ok, <<"Item price updated successfully">>};
                {error, Reason} ->
                    {error, Reason}
            end;
        {error, <<"Item not found">>} ->
            {error, <<"Item not found">>};
        {error, Reason} ->
            {error, Reason}
    end.