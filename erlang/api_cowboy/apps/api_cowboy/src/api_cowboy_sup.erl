-module(api_cowboy_sup).
-behaviour(supervisor).

-export([start_link/1, init/1]).
-define(SERVER, ?MODULE).

start_link(Dispatch) ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, [Dispatch]).

init([Dispatch]) ->
    SupFlags = #{
        strategy => one_for_one,
        intensity => 1,
        period => 5
    },
    ChildSpecs = [
        #{id => http_listener,
          start => {cowboy, start_clear, [http_listener, [{port, 8080}], #{env => #{dispatch => Dispatch}}]},
          restart => permanent,
          shutdown => 5000,
          type => worker,
          modules => [cowboy]}
    ],
    {ok, {SupFlags, ChildSpecs}}.
