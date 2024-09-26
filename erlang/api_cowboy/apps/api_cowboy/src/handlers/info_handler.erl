-module(info_handler).
-export([get_item/1, get_all_items/0, create_item/2, 
    update_item/2, delete_item/1, patch_item_price/2,
    exists_item/1]).

get_item(ItemName) ->
    case db_operations:get_item(ItemName) of
        {ok, Value} ->
            {ok, Value};
        {error, Reason} ->
            {error, Reason}
    end.

exists_item(ItemName) ->
    case db_operations:get_item(ItemName) of
        {ok, Value} when Value =/= undefined ->
            {ok, true};
        {ok, undefined} ->
            {ok, false}
    end.

get_all_items() ->
    case db_operations:get_all_items() of
        {ok, Items} ->
            ItemsWithKeys = [#{<<"key">> => Key, <<"value">> => Value} || {Key, Value} <- Items],
            Json = jsx:encode(ItemsWithKeys),
            {ok, Json};
        {error, Reason} ->
            EncodedReason = jsx:encode([{<<"error">>, Reason}]),
            {error, EncodedReason}
    end.

create_item(ItemName, ItemParsed) ->
    case db_operations:create_item(ItemName, ItemParsed) of
        {ok, <<"OK">>} ->
            {ok, true};
        {error, Reason} ->
            {error, Reason}
    end.

update_item(ItemName, ItemParsed) ->
    case db_operations:update_item(ItemName, ItemParsed) of
        {ok, <<"OK">>} ->
            {ok, true};
        {error, Reason} ->
            {error, Reason}
    end.

delete_item(ItemName) ->
    case db_operations:delete_item(ItemName) of
        {ok, <<"1">>} ->
            {ok, true};
        {error, Reason} ->
            {error, Reason}
    end.

patch_item_price(ItemName, NewPrice) ->
    case get_item(ItemName) of 
        {ok, CurrentItem} ->
            ParsedCurrentItem = maps:from_list(jsx:decode(CurrentItem)),
            UpdatedItem = maps:put(<<"price">>, NewPrice, ParsedCurrentItem),
            case db_operations:update_item(ItemName, UpdatedItem) of
                {ok, <<"OK">>} ->
                    {ok, true};
                {error, Reason} ->
                    {error, Reason}
            end;
        {error, Reason} ->
            {error, Reason}
    end.
