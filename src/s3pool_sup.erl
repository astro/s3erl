-module(s3pool_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).
-define(WORKERS, 40).
-include("../deps/s3erl/include/s3.hrl").
-define(CREDENTIALS, #aws_credentials{accessKeyId="",
                secretAccessKey=""}).

%%%===================================================================
%%% API functions
%%%===================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, [?WORKERS]).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

init([N]) ->
    RestartStrategy = one_for_one,
    MaxRestarts = 100,
    MaxSecondsBetweenRestarts = 10,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    Restart = permanent,
    Shutdown = 5000,
    Type = worker,

    Workers = [{{s3, I}, {s3, start, [?CREDENTIALS]},
		Restart, Shutdown, Type, []}
	       || I <- lists:seq(1, N)],
    {ok, {SupFlags, Workers}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
