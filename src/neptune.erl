-module(neptune).

-include("neptune.hrl").
-include("neptune_limits.hrl").

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

-endif.

-export([main/0, main/1, double_up/1, square/1]).

-on_load(init/0).

init() ->
    PrivDir = case code:priv_dir(?MODULE) of
		  {error, bad_name} ->
		      BeamPath = code:where_is_file("neptune.beam"),
		      BeamDir = filename:dirname(BeamPath),
		      AppDir = filename:dirname(BeamDir),
		      filename:join(AppDir, ".libs");
		  Dir ->
		      Dir
	      end,
    Path = filename:join(PrivDir, "libneptune_nif"),
    ok = erlang:load_nif(Path, 0).

main() ->
    io:format("~s~n", [greeting(?DEFAULT_SALUTATION)]).

main(Salutation)
  when size(Salutation) > ?MAX_SALUTATION_LENGTH ->
    {error, max_salutation_length};
main(Salutation) ->
    io:format("~s~n", [greeting(Salutation)]).

greeting(Salutation) ->
    Place = atom_to_binary(?MODULE),
    <<Salutation/binary, " from ", Place/binary, "!">>.

%% NIF Functions
double_up(_X) ->
    exit(nif_library_not_loaded).

square(_Y) ->
    exit(nif_library_not_loaded).

-ifdef(TEST).

greeting_test_() ->
    [?_assertMatch(<<"Hello from neptune!">>, greeting(<<"Hello">>)),
     ?_assertMatch(<<"Hi there from neptune!">>, greeting(<<"Hi there">>))].

double_up_test_() ->
    [?_assertMatch(6,  double_up(3)),
     ?_assertMatch(12, double_up(6))].

square_test_() ->
    [?_assertMatch(9,  square(3)),
     ?_assertMatch(36, square(6))].

-endif.
