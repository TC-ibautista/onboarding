-module(api_cowboy_app).
-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    Routes = [
        {"/", root_handler, []},
        {"/api/v1/items", items_handler, []},
        {"/api/v1/items/:name", items_handler, [has_name]}
    ],
    Dispatch = cowboy_router:compile([{'_', Routes}]),
    api_cowboy_sup:start_link(Dispatch).

stop(_State) ->
    ok.
