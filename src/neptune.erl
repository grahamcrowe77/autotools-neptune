-module(neptune).

-include("neptune.hrl").

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

-endif.

-export([main/0, main/1]).

main() ->
    io:format("~s~n", [greeting(?DEFAULT_SALUTATION)]).

main(Salutation) ->
    io:format("~s~n", [greeting(Salutation)]).

greeting(Salutation) ->
    Place = atom_to_binary(?MODULE),
    <<Salutation/binary, " from ", Place/binary, "!">>.

-ifdef(TEST).

greeting_test_() ->
    [?_assertMatch(
	<<"Hello from neptune!">>,
	greeting(<<"Hello">>)),
     ?_assertMatch(
	<<"Hi there from neptune!">>,
	greeting(<<"Hi there">>))
    ].
-endif.
