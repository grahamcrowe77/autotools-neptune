%% @author Graham Crowe <graham.crowe@telia.com>
%% @copyright 2021 Graham Crowe
%% @doc This application demonstrates using GNU Autotools.
%%
%% This application demonstrates using GNU Autotools to build Erlang
%% applications, including C code that can be cross compiled according
%% to the build environment. It includes examples of Autotest for
%% running `Dialyzer', `Eunit' and `Common' tests using the standard
%% GNU make target `check'. It includes `install' and `installcheck'
%% targets that work with `DESTDIR' to support installations to a
%% staging directory for packaging. It also includes `dist' and
%% `distcheck' targets for generating distribution tarballs.
%% @end

-module(neptune).

-include("neptune.hrl").
-include("neptune_limits.hrl").

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

-endif.

-on_load(init/0).

-export([main/0, main/1]).

-export([double_up/1, square/1]).

%% -------------------------------------------------------------------
%% On load functions
%% -------------------------------------------------------------------
%% @doc Initialize the module on loading.

-spec init() -> ok.

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

%% -------------------------------------------------------------------

%% -------------------------------------------------------------------
%% Exported functions
%% -------------------------------------------------------------------
%% @doc Write a default greeting to the IoDevice.

-spec main() -> ok | {error, Error} when
      Error :: atom().

main() ->
    io:format("~s~n", [greeting(?DEFAULT_SALUTATION)]).

%% @doc Write a user defined greeting to the IoDevice.

main(Salutation)
  when size(Salutation) > ?MAX_SALUTATION_LENGTH ->
    {error, max_salutation_length};
main(Salutation) ->
    io:format("~s~n", [greeting(Salutation)]).

%% -------------------------------------------------------------------

%% -------------------------------------------------------------------
%% Internal functions
%% -------------------------------------------------------------------
%% @doc Return a greeting binary string.

-spec greeting(binary()) -> binary().

greeting(Salutation) ->
    Place = atom_to_binary(?MODULE),
    <<Salutation/binary, " from ", Place/binary, "!">>.

%% -------------------------------------------------------------------

%% -------------------------------------------------------------------
%% NIF functions
%% -------------------------------------------------------------------

%% -------------------------------------------------------------------
%% @doc Double an integer.

-spec double_up(integer()) -> integer().

double_up(_X) ->
    exit(nif_library_not_loaded).

%% -------------------------------------------------------------------

%% -------------------------------------------------------------------
%% @doc Square an integer.

-spec square(integer()) -> integer().

square(_Y) ->
    exit(nif_library_not_loaded).

%% -------------------------------------------------------------------

%% -------------------------------------------------------------------
%% Internal eunit tests
%% -------------------------------------------------------------------
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
%% -------------------------------------------------------------------
