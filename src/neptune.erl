-module(neptune).

-include("neptune.hrl").

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

-endif.

-export([main/0, main/1, foo/1, bar/1]).

-on_load(init/0).

init() ->
    PrivDir = case code:priv_dir(?MODULE) of
		  {error, bad_name} ->
		      BeamPath = code:where_is_file("neptune.beam"),
		      BeamDir = filename:dirname(BeamPath),
		      AppDir = filename:dirname(BeamDir),
		      filename:join(AppDir, "c_src/.libs");
		  Dir ->
		      Dir
	      end,
    Path = filename:join(PrivDir, "libneptune_nif"),
    ok = erlang:load_nif(Path, 0).

main() ->
    io:format("~s~n", [greeting(?DEFAULT_SALUTATION)]).

main(Salutation) ->
    io:format("~s~n", [greeting(Salutation)]).

foo(_X) ->
    exit(nif_library_not_loaded).

bar(_Y) ->
    exit(nif_library_not_loaded).

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
