-module(api_functions).

-export([get_all_items/2, get_item/3, post_item/2, put_item/3,
patch_item_price/3, delete_item/3]).

get_all_items(Req, State) ->
    case info_handler:get_all_items() of
        {ok, Response} ->
            {Response, Req, State};
        {error, Reason} ->
            {Reason, Req, State}
    end.

get_item(Req, State, ItemName) ->
    case info_handler:get_item(ItemName) of
        {ok, Response } ->
            {Response, Req, State};
        {error, Reason} ->
            {Reason, Req, State}
    end.

post_item(Req, State) ->
    {ok, ItemBody, Req2} = cowboy_req:read_body(Req),
    ItemParsed = jsx:decode(ItemBody, [return_maps]),
    ItemName = maps:get(<<"name">>, ItemParsed),
    case info_handler:create_item(ItemName, ItemParsed) of
        {ok, Response} ->
            SuccessMsg = #{<<"message">> => <<"Item created successfully">>},
            Req3 = cowboy_req:set_resp_body(jsx:encode(SuccessMsg), Req2),
            {Response, Req3, State};
        {error, Reason} ->
            {Reason, Req2, State}
    end.

put_item(Req, State, ItemName) ->
    {ok, ItemBody, Req2} = cowboy_req:read_body(Req),
    ItemParsed = jsx:decode(ItemBody, [return_maps]),
    case info_handler:update_item(ItemName, ItemParsed) of
        {ok, Response} ->
            SuccessMsg = #{<<"message">> => <<"Item updated successfully">>},
            Req3 = cowboy_req:set_resp_body(jsx:encode(SuccessMsg), Req2),
            {Response, Req3, State};
        {error, Reason} ->
            {Reason, Req2, State}
    end.

patch_item_price(Req, State, ItemName) ->
    {ok, ItemBody, Req2} = cowboy_req:read_body(Req),
    ItemParsed = jsx:decode(ItemBody, [return_maps]),
    NewPrice = maps:get(<<"price">>, ItemParsed),
    case info_handler:patch_item_price(ItemName, NewPrice) of
        {ok, Response} ->
            SuccessMsg = #{<<"message">> => <<"Item price updated successfully">>},
            Req3 = cowboy_req:set_resp_body(jsx:encode(SuccessMsg), Req2),
            {Response, Req3, State};
        {error, Reason} ->
            {Reason, Req2, State}
    end.

delete_item(Req, State, ItemName) ->
    case info_handler:delete_item(ItemName) of
        {ok, true} ->
            SuccessMsg = #{<<"message">> => <<"Item deleted successfully">>},
            Req2 = cowboy_req:set_resp_body(jsx:encode(SuccessMsg), Req),
            {true, Req2, State};
        {error, Reason} ->
            ErrorMsg = #{<<"message">> => Reason},
            Req2 = cowboy_req:set_resp_body(jsx:encode(ErrorMsg), Req),
            {false, Req2, State}
    end.
