-module(redis_pool).
-behaviour(supervisor).

-export([start_link/0, init/1, checkout/0, checkin/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    RedisHost = os:getenv("REDIS_HOST"),
    RedisPort = os:getenv("REDIS_PORT"),
    RedisPoolSize = os:getenv("REDIS_POOL_SIZE"),
    PoolArgs = [
        {name, {local, redis_pool}},
        {worker_module, eredis},
        {size, RedisPoolSize},
        {max_overflow, 0}
    ],
    ChildSpec = poolboy:child_spec(redis_pool, PoolArgs, [{host, RedisHost}, {port, RedisPort}]),
    {ok, {{one_for_one, 5, 10}, [ChildSpec]}}.

checkout() ->
    poolboy:checkout(redis_pool).

checkin(Client) ->
    poolboy:checkin(redis_pool, Client).
