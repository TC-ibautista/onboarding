-module(api_functions_test).
-include_lib("eunit/include/eunit.hrl").

setup() ->
    {ok, _} = application:ensure_all_started(api_cowboy).

exists_item_success_test() ->
    % Arrange
    setup(),
    ItemName = "Item_1",
    ExpectedOutput = {ok, true},

    meck:new(db_operations, [passthrough]),
    meck:expect(db_operations, get_item, fun(_ItemName) -> {ok, true} end),

    % Act
    ActualOutput = info_handler:exists_item(ItemName),

    % Assert
    ?assertEqual(ExpectedOutput, ActualOutput),
    meck:unload().

does_not_exist_item_test () ->
    % Arrange
    setup(),
    ItemName = "Item_not_found",
    ExpectedOutput = {ok, false},

    meck:new(db_operations, [passthrough]),
    meck:expect(db_operations, get_item, fun(_ItemName) -> {ok, undefined} end),

    % Act
    ActualOutput = info_handler:exists_item(ItemName),

    % Assert
    ?assertEqual(ExpectedOutput, ActualOutput),
    meck:unload().

get_item_success_test() ->
    % Arrange
    setup(),
    ItemName = "Item_1",
    ExpectedOutput = {ok,<<"{\"name\":\"Item_1\",\"price\":123}">>},

    meck:new(db_operations, [passthrough]),
    meck:expect(db_operations, get_item, fun(_ItemName) -> {ok,<<"{\"name\":\"Item_1\",\"price\":123}">>} end),

    % Act
    ActualOutput = info_handler:get_item(ItemName),

    % Assert
    ?assertEqual(ExpectedOutput, ActualOutput),
    meck:unload().

get_item_undefined_test() ->
    % Arrange
    setup(),
    ItemName = "Item_not_found",
    ExpectedOutput = {ok, undefined},

    meck:new(db_operations, [passthrough]),
    meck:expect(db_operations, get_item, fun(_ItemName) -> {ok, undefined} end),

    % Act
    ActualOutput = info_handler:get_item(ItemName),

    % Assert
    ?assertEqual(ExpectedOutput, ActualOutput),
    meck:unload().

get_all_items_success_test() ->
    % Arrange
    setup(),
    ExpectedOutput = {ok,<<"[{\"key\":\"Item_1\",\"value\":{\"name\":\"Item_1\",\"price\":123}}]">>},

    meck:new(db_operations, [passthrough]),
    meck:expect(db_operations, get_all_items, fun() -> 
        {ok,[{<<"Item_1">>,[{<<"name">>,<<"Item_1">>},{<<"price">>,123}]}]}
    end),

    % Act
    ActualOutput = info_handler:get_all_items(),

    % Assert
    ?assertEqual(ExpectedOutput, ActualOutput),
    meck:unload().

create_item_success_test() ->
    % Arrange
    setup(),
    ItemName = "Item_1",
    ItemParsed = #{<<"name">> => <<"Item_1">>, <<"price">> => 123},
    ExpectedOutput = {ok, true},

    meck:new(db_operations, [passthrough]),
    meck:expect(db_operations, create_item, fun(_ItemName, _ItemParsed) -> {ok, <<"OK">>} end),

    % Act
    ActualOutput = info_handler:create_item(ItemName, ItemParsed),

    % Assert
    ?assertEqual(ExpectedOutput, ActualOutput),
    meck:unload().

update_item_success_test() -> 
    % Arrange
    setup(),
    ItemName = "Item_1",
    ItemParsed = #{<<"name">> => <<"Item_1">>, <<"price">> => 123, <<"description">> => <<"Item description">>},
    ExpectedOutput = {ok, true},

    meck:new(db_operations, [passthrough]),
    meck:expect(db_operations, update_item, fun(_ItemName, _ItemParsed) -> {ok, <<"OK">>} end),

    % Act
    ActualOutput = info_handler:update_item(ItemName, ItemParsed),

    % Assert
    ?assertEqual(ExpectedOutput, ActualOutput),
    meck:unload().

delete_item_success_test() -> 
    % Arrange
    setup(),
    ItemName = "Item_1",
    ExpectedOutput = {ok, true},

    meck:new(db_operations, [passthrough]),
    meck:expect(db_operations, delete_item, fun(_ItemName) -> {ok, <<"1">>} end),

    % Act
    ActualOutput = info_handler:delete_item(ItemName),

    % Assert
    ?assertEqual(ExpectedOutput, ActualOutput),
    meck:unload().

patch_item_price_success_test() -> 
    % Arrange
    setup(),
    ItemName = "Item_1",
    NewPrice = 456,
    ExpectedOutput = {ok, true},

    meck:new(db_operations, [passthrough]),
    meck:expect(db_operations, get_item, fun(_ItemName) -> {ok,<<"{\"name\":\"Item_1\",\"price\":123}">>} end),
    meck:expect(db_operations, update_item, fun(_ItemName, _ItemParsed) -> {ok, <<"OK">>} end),

    % Act
    ActualOutput = info_handler:patch_item_price(ItemName, NewPrice),

    % Assert
    ?assertEqual(ExpectedOutput, ActualOutput),
    meck:unload().
