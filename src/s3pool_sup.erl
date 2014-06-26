-module(s3pool_sup).

-behaviour(supervisor).

%% API
-export([start_link/0, start_link/1, start_link/2]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).
-define(WORKERS, 40).

%%%===================================================================
%%% API functions
%%%===================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, [undefined, ?WORKERS]).
start_link(Credentials) ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, [Credentials, ?WORKERS]).
start_link(Credentials, N) ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, [Credentials, N]).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

init([Credentials, N]) ->
    RestartStrategy = one_for_one,
    MaxRestarts = 100,
    MaxSecondsBetweenRestarts = 10,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    Restart = permanent,
    Shutdown = 5000,
    Type = worker,

    Pool = {pool, {s3pool, start_link, [Credentials]},
	    Restart, Shutdown, Type, [s3pool]},
    Workers = [{{s3, I}, {ibrowse_http_client, start_link, [{"s3.amazonaws.com", 80}]},
		Restart, Shutdown, Type, [ibrowse_http_client]}
	       || I <- lists:seq(1, N)],
    {ok, {SupFlags, [Pool | Workers]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
