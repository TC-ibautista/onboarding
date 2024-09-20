-module(api_cowboy_app).
-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    Routes = [
        {"/", api_cowboy_handler, []},
        {"/api/v1/items", api_cowboy_handler, []},
        {"/api/v1/items/:name", api_cowboy_handler, []}
    ],
    Dispatch = cowboy_router:compile([{'_', Routes}]),
    api_cowboy_sup:start_link(Dispatch).

stop(_State) ->
    ok.
