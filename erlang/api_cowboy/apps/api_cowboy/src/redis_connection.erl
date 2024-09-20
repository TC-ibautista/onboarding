-module(redis_connection).
-export([connect/0]).

connect() ->
    RedisHost = os:getenv("REDIS_HOST"),
    RedisPort = os:getenv("REDIS_PORT"),
    case eredis:start_link([{host, RedisHost}, {port, list_to_integer(RedisPort)}]) of
        {ok, Client} ->
            {ok, Client};
        {error, Reason} ->
            io:format("Failed to start Redis client: ~p~n", [Reason]),
            {error, Reason}
    end.
