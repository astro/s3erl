-module(s3app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    s3pool_sup:start_link().

stop(_State) ->
    ok.
