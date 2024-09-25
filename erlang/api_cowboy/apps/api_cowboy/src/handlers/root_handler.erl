-module(root_handler).

-export([init/2, allowed_methods/2, 
    content_types_provided/2, to_json/2]).

init(Req, State) ->
    {cowboy_rest, Req, State}.

allowed_methods(Req, State) ->
    {[<<"GET">>, <<"OPTIONS">>], Req, State}.

content_types_provided(Req, State) ->
    {[{<<"application/json">>, to_json}], Req, State}.

to_json(Req, State) ->
    Response = <<"{\"message\": \"Welcome to the API Cowboy!\"}">>,
    {Response, Req, State}.
