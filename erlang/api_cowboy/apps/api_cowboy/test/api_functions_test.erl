-module(api_functions_test).
-include_lib("eunit/include/eunit.hrl").

setup() ->
    {ok, _} = application:ensure_all_started(api_cowboy).

exists_item_success_test() ->
    % Arrange
    setup(),
    ItemName = "Palanqueta",
    ExpectedOutput = {ok,true},
    % Act
    ActualOutput = info_handler:exists_item(ItemName),
    % Assert
    ?assertEqual(ExpectedOutput, ActualOutput).

does_not_exist_item_test () ->
    % Arrange
    setup(),
    ItemName = "NotFound",
    ExpectedOutput = {ok,false},
    % Act
    ActualOutput = info_handler:exists_item(ItemName),
    % Assert
    ?assertEqual(ExpectedOutput, ActualOutput).

get_item_success_test() ->
    % Arrange
    setup(),
    ItemName = "Palanqueta",
    ExpectedOutput = {ok,<<"{\"name\":\"Palanqueta\",\"price\":198}">>},
    % Act
    ActualOutput = info_handler:get_item(ItemName),
    % Assert
    ?assertEqual(ExpectedOutput, ActualOutput).

get_item_undefined_test() ->
    % Arrange
    setup(),
    ItemName = "NotFound",
    ExpectedOutput = {ok, undefined},
    % Act
    ActualOutput = info_handler:get_item(ItemName),
    % Assert
    ?assertEqual(ExpectedOutput, ActualOutput).
