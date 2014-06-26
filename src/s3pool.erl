-module(s3pool).

%% API
-export([get_credentials/0, set_credentials/1, get_worker/0, start_link/1, loop/2]).

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

set_credentials(Credentials) ->
    Ref = make_ref(),
    ?SERVER ! {set_credentials, Ref, self(), Credentials},
    receive
	{ok, Ref} ->
	    ok
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

start_link() ->
    start_link(undefined).

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
        {set_credentials, Ref, From, NewCredentials} ->
	    From ! {ok, Ref},
            loop(NewCredentials, [Worker | Workers]);
	{credentials, Ref, From} ->
	    From ! {ok, Ref, Credentials},
            loop(Credentials, Workers);
	{next, Ref, From} ->
            From ! {ok, Ref, Worker},
            loop(Credentials, Workers)
    end.
