-module(api_cowboy_handler).
-behaviour(cowboy_handler).

-export([init/2]).

init(Req, State) ->
    {ok, Req2} = cowboy_req:reply(200,
        #{<<"content-type">> => <<"text/plain">>},
        <<"Hello, Cowboy!">>,
        Req),
    {ok, Req2, State}.