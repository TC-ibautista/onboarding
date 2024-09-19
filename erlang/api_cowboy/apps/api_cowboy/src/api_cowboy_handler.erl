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
    send_response(Req0, 200, <<"Welcome to the API Cowboy!">>, <<"text/plain">>, State).

handle_get_items(Req0, State) ->
    case info_handler:get_all_items() of
        {ok, Response} ->
            send_response(Req0, 200, Response, <<"application/json">>, State);
        {error, Reason} ->
            send_response(Req0, 500, Reason, <<"text/plain">>, State)
    end.

handle_get_item(Req0, State, ItemName) ->
    case info_handler:get_item(ItemName) of
        {ok, Response } ->
            send_response(Req0, 200, Response, <<"application/json">>, State);
        {error, <<"Item not found">>} ->
            send_response(Req0, 404, <<"Item not found">>, <<"text/plain">>, State);
        {error, Reason} ->
            send_response(Req0, 500, Reason, <<"application/json">>, State)
    end.

handle_post_items(Req0, State) ->
    {ok, Body, _} = cowboy_req:read_body(Req0),
    case info_handler:create_item(Body) of
        {ok, Response} ->
            send_response(Req0, 201, Response, <<"application/json">>, State);
        {error, Reason} ->
            send_response(Req0, 500, Reason, <<"text/plain">>, State)
    end.

handle_put_item(Req0, State, ItemName) ->
    {ok, Body, _} = cowboy_req:read_body(Req0),
    case info_handler:update_item(ItemName, Body) of
        {ok, Response} ->
            send_response(Req0, 200, Response, <<"application/json">>, State);
        {error, Reason} ->
            send_response(Req0, 500, Reason, <<"application/json">>, State)
    end.

handle_patch_item_price(Req0, State, ItemName) ->
    {ok, Body, _} = cowboy_req:read_body(Req0),
    case info_handler:patch_item_price(ItemName, Body) of
        {ok, Response} ->
            send_response(Req0, 200, Response, <<"application/json">>, State);
        {error, <<"Item not found">>} ->
            send_response(Req0, 404, <<"Item not found">>, <<"text/plain">>, State);
        {error, Reason} ->
            send_response(Req0, 500, Reason, <<"application/json">>, State)
    end.

handle_delete_item(Req0, State, ItemName) ->
    case info_handler:delete_item(ItemName) of
        {ok, Response} ->
            send_response(Req0, 200, Response, <<"application/json">>, State);
        {error, <<"Item not found">>} ->
            send_response(Req0, 404, <<"Item not found">>, <<"text/plain">>, State);
        {error, Reason} ->
            send_response(Req0, 500, Reason, <<"application/json">>, State)
    end.

handle_method_not_allowed(Req0, State) ->
    send_response(Req0, 405, <<"Method Not Allowed">>, <<"text/plain">>, State).

send_response(Req0, StatusCode, Body, ContentType, State) ->
    Req = cowboy_req:reply(StatusCode, #{<<"content-type">> => ContentType}, Body, Req0),
    {ok, Req, State}.