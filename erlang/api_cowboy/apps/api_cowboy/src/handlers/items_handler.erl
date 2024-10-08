-module(items_handler).

-export([init/2, allowed_methods/2, content_types_accepted/2, 
    content_types_provided/2, from_json/2, to_json/2, 
    delete_resource/2, resource_exists/2]).

init(Req, State) ->
    {cowboy_rest, Req, State}.

allowed_methods(Req, State) ->
    {[<<"GET">>, <<"POST">>, <<"PUT">>, <<"PATCH">>, <<"DELETE">> , <<"OPTIONS">>], Req, State}.

resource_exists(Req, State = [has_name]) ->
    ItemName = cowboy_req:binding(name, Req),
    case info_handler:exists_item(ItemName) of
        {ok, true} ->
            {true, Req, State};
        {ok, false} ->
            {false, Req, State}
    end;
resource_exists(Req, State) ->
    {true, Req, State}.

content_types_accepted(Req, State) ->
    {[{<<"application/json">>, from_json}], Req, State}.

content_types_provided(Req, State) ->
    {[{<<"application/json">>, to_json}], Req, State}.

from_json(Req, State = [has_name]) ->
    ItemName = cowboy_req:binding(name, Req),
    Method = cowboy_req:method(Req),
    {ok, Body, Req2} = cowboy_req:read_body(Req),
    case validator:validate_request(Req2, Body) of
        {ok, true} ->
            case Method of
                <<"PUT">> ->
                    api_functions:put_item(Req2, State, ItemName, Body);
                <<"PATCH">> ->
                    api_functions:patch_item_price(Req2, State, ItemName, Body)
            end;
        {error, Reason} ->
            send_errMessage(Req2, State, Reason)
    end;
from_json(Req, State) ->
    {ok, Body, Req2} = cowboy_req:read_body(Req),
    case validator:validate_request(Req2, Body) of
        {ok, true} ->
            api_functions:post_item(Req2, State, Body);
        {error, Reason} ->
            send_errMessage(Req2, State, Reason)
    end.

to_json(Req, State = [has_name]) ->
    ItemName = cowboy_req:binding(name, Req),
    api_functions:get_item(Req, State, ItemName);
to_json(Req, State) ->
    api_functions:get_all_items(Req, State).

delete_resource(Req, State = [has_name]) ->
    ItemName = cowboy_req:binding(name, Req),
    api_functions:delete_item(Req, State, ItemName);
delete_resource(Req, State) ->
    NoNameMessage = #{<<"message">> => <<"No item name binding found">>},
    send_errMessage(Req, State, NoNameMessage).

send_errMessage(Req, State, Message) ->
    Req2 = cowboy_req:set_resp_body(jsx:encode(Message), Req),
    {false, Req2, State}.
