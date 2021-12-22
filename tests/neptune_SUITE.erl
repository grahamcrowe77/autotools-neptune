-module(neptune_SUITE).

-include("neptune.hrl").
-include("neptune_limits.hrl").

-compile(export_all).

all() ->
    [main0,
     main1].

main0(_) ->
    ok = neptune:main().

main1(_) ->
    ok = neptune:main(?INFORMAL_SALUTATION),
    ok = neptune:main(<<"How are you">>),
    LongString = lists:duplicate($a, ?MAX_SALUTATION_LENGTH + 1),
    LongGreeting = list_to_binary(LongString),
    {error, max_salutation_length} = neptune:main(LongGreeting),
    ok.
