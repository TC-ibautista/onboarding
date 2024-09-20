-module(redis_connection).
-export([connect/0]).

connect() ->
    {ok, RedisHost} = application:get_env(my_app, redis_host),
    {ok, RedisPort} = application:get_env(my_app, redis_port),
    case eredis:start_link([{host, RedisHost}, {port, RedisPort}]) of
        {ok, Client} ->
            {ok, Client};
        {error, Reason} ->
            io:format("Failed to start Redis client: ~p~n", [Reason]),
            {error, Reason}
    end.
