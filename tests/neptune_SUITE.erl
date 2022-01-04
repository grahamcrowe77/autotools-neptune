-module(neptune_SUITE).

-include("neptune.hrl").
-include("neptune_limits.hrl").

-compile(export_all).

all() ->
    [write0,
     write1,
     application].

write0(_) ->
    ok = neptune:write().

write1(_) ->
    ok = neptune:write(?INFORMAL_SALUTATION),
    ok = neptune:write(<<"How are you">>),
    LongString = lists:duplicate($a, ?MAX_SALUTATION_LENGTH + 1),
    LongGreeting = list_to_binary(LongString),
    {error, max_salutation_length} = neptune:write(LongGreeting),
    ok.

application(_) ->
    false = lists:keyfind(neptune, 1, application:which_applications()),
    ok = application:start(neptune),
    {neptune, "Neptune application", _} =
	lists:keyfind(neptune, 1, application:which_applications()),
    ok = application:stop(neptune),
    false = lists:keyfind(neptune, 1, application:which_applications()),
    ok.
