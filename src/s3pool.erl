-module(s3pool).

%% API
-export([get_worker/0, start_link/0, loop/1]).

-define(SERVER, ?MODULE).
-define(POOL, s3pool_sup).

%%%===================================================================
%%% API functions
%%%===================================================================
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
    Pid = spawn_link(fun() ->
			     register(?SERVER, self()),
			     ?MODULE:loop([])
		     end),
    {ok, Pid}.


%%%===================================================================
%%% Internal functions
%%%===================================================================

loop([]) ->
    Workers =
	[Child
	 || {_, Child, _, _} <- supervisor:which_children(?POOL),
	    is_pid(Child)],
    ?MODULE:loop(Workers);
loop([Worker | Workers]) ->
    receive
	{next, Ref, From} ->
	    From ! {ok, Ref, Worker}
    end,
    loop(Workers).
