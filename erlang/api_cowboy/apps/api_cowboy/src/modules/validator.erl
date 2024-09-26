-module(validator).

-export([validate_request/2]).

validate_request(Req, Body) ->
    Method = cowboy_req:method(Req),
    case Method of
        <<"POST">> ->
            validate_post_request(Req, Body);
        <<"PUT">> ->
            validate_put_request(Req, Body);
        <<"PATCH">> ->
            validate_patch_request(Req, Body)
    end.

validate_patch_request(Req, Body) ->
    Bindings = cowboy_req:bindings(Req),
    case maps:is_key(name, Bindings) of
        true ->
            case jsx:decode(Body, [return_maps]) of
                #{<<"price">> := Price} ->
                    validate_patch_fields(Price);
                _ ->
                    {error, #{<<"message">> => <<"Invalid PATCH data">>}}
            end;
        false ->
            {error, #{<<"message">> => <<"Bindings must include 'name' for PATCH requests">>}}
    end.

validate_put_request(Req, Body) ->
    Bindings = cowboy_req:bindings(Req),
    case maps:is_key(name, Bindings) of
        true ->
            case jsx:decode(Body, [return_maps]) of
                #{<<"name">> := Name, <<"price">> := Price} ->
                    validate_put_fields(Name, Price);
                _ ->
                    {error, #{<<"message">> => <<"Invalid PUT data">>}}
            end;
        false ->
            {error, #{<<"message">> => <<"Bindings must include 'name' for PUT requests">>}}
    end.

validate_post_request(Req, Body) ->
    Bindings = cowboy_req:bindings(Req),
    case Bindings of
        #{} ->
            case jsx:decode(Body, [return_maps]) of
                #{<<"name">> := Name, <<"price">> := Price} ->
                    validate_post_fields(Name, Price);
                _ ->
                    {error, #{<<"message">> => <<"Invalid POST data">>}}
            end;
        _ ->
            {error, #{<<"message">> => <<"Bindings should be empty for POST requests">>}}
    end.

validate_post_fields(Name, Price) when is_binary(Name), is_number(Price), Price >= 0, Name =/= <<>>, Name =/= <<" ">> ->
    {ok, true};
validate_post_fields(_, _) ->
    {error, #{<<"message">> => <<"Invalid POST data: 'name' must be a non-empty string and 'price' must be a non-negative number">>}}.

validate_put_fields(Name, Price) when is_binary(Name), is_number(Price), Price >= 0, Name =/= <<>>, Name =/= <<" ">> ->
    {ok, true};
validate_put_fields(_, _) ->
    {error, #{<<"message">> => <<"Invalid PUT data: 'name' must be a non-empty string and 'price' must be a non-negative number">>}}.

validate_patch_fields(Price) when is_number(Price), Price >= 0 ->
    {ok, true};
validate_patch_fields(_) ->
    {error, #{<<"message">> => <<"Invalid PATCH data: 'price' must be a non-negative number">>}}.
