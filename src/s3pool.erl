-module(s3pool).

%% API
-export([get_credentials/0, get_worker/0, start_link/1, loop/2]).

-define(SERVER, ?MODULE).
-define(POOL, s3pool_sup).

%%%===================================================================
%%% API functions
%%%===================================================================
get_credentials() ->
    Ref = make_ref(),
    ?SERVER ! {credentials, Ref, self()},
    receive
	{ok, Ref, Credentials} ->
	    {ok, Credentials}
    after 5000 ->
	    exit(s3pool_timeout)
    end.
    

get_worker() ->
    Ref = make_ref(),
    ?SERVER ! {next, Ref, self()},
    receive
	{ok, Ref, Worker} ->
	    {ok, Worker}
    after 5000 ->
	    exit(s3pool_timeout)
    end.

start_link(Credentials) ->
    Pid = spawn_link(fun() ->
			     register(?SERVER, self()),
			     ?MODULE:loop(Credentials, [])
		     end),
    {ok, Pid}.


%%%===================================================================
%%% Internal functions
%%%===================================================================

loop(Credentials, []) ->
    Workers =
	[Child
	 || {{s3, _}, Child, _, _} <- supervisor:which_children(?POOL),
	    is_pid(Child)],
    ?MODULE:loop(Credentials, Workers);

loop(Credentials, [Worker | Workers]) ->
    receive
	{credentials, Ref, From} ->
	    From ! {ok, Ref, Credentials};
	{next, Ref, From} ->
	    From ! {ok, Ref, Worker}
    end,
    loop(Credentials, Workers).
