-module(neptune_SUITE).

-include("neptune.hrl").
-include("neptune_limits.hrl").

-compile(export_all).

all() ->
    [application,
     version,
     square_integer,
     square_string].

write1(_) ->
    ok = neptune_io:write(?INFORMAL_SALUTATION),
    ok = neptune_io:write(<<"How are you">>),
    LongString = lists:duplicate($a, ?MAX_SALUTATION_LENGTH + 1),
    LongGreeting = list_to_binary(LongString),
    {error, max_salutation_length} = neptune_io:write(LongGreeting),
    ok.

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
