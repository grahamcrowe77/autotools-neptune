-module(neptune_SUITE).

-include("neptune.hrl").
-include("neptune_priv.hrl").

-compile(export_all).

all() ->
    [application,
     version,
     square_integer,
     square_string].

application(_) ->
    false = lists:keyfind(neptune, 1, application:which_applications()),
    ok = application:start(neptune),
    {neptune, "Neptune application", _} =
	lists:keyfind(neptune, 1, application:which_applications()),
    ok = application:stop(neptune),
    false = lists:keyfind(neptune, 1, application:which_applications()),
    ok.

version(_) ->
    ok = application:start(neptune),
    {ok, Version} = neptune_server:version(),
    ok = ct:pal("Neptune Version: ~s", [Version]),
    ok = application:stop(neptune).

square_integer(_) ->
    ok = application:start(neptune),
    {ok, Message} = neptune_server:message({square, 6}),
    ok = ct:pal("~s", [Message]),
    ok = application:stop(neptune).

square_string(_) ->
    ok = application:start(neptune),
    {ok, Message} = neptune_server:message({square, "6"}),
    ok = ct:pal("~s", [Message]),
    ok = application:stop(neptune).
