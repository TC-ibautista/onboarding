-module(api_cowboy_app).
-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/cowboy", api_cowboy_handler, []},
            {"/api/v1/items", api_cowboy_handler, []},
            {"/api/v1/items/:id", api_cowboy_handler, []}
        ]}
    ]),
    api_cowboy_sup:start_link(Dispatch).

stop(_State) ->
    ok.