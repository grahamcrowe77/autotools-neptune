%% @author Graham Crowe <graham.crowe@telia.com>
%% @copyright 2021 Graham Crowe
%% @doc This module demonstrates using GNU Autotools to build NIFs.
%%
%% @end

-module(neptune_nif).

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

-endif.

-on_load(init/0).

-export([double_up/1, square/1]).

%% -------------------------------------------------------------------
%% On load functions
%% -------------------------------------------------------------------
%% @doc Initialize the module on loading.

-spec init() -> ok.

init() ->
    PrivDir = case code:priv_dir(?MODULE) of
		  {error, bad_name} ->
		      BeamPath = code:where_is_file("neptune_nif.beam"),
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
%% @doc Double an integer.

-spec double_up(integer()) -> integer().

double_up(_X) ->
    exit(nif_library_not_loaded).

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

double_up_test_() ->
    [?_assertMatch(6,  double_up(3)),
     ?_assertMatch(12, double_up(6))].

square_test_() ->
    [?_assertMatch(9,  square(3)),
     ?_assertMatch(36, square(6))].

-endif.
%% -------------------------------------------------------------------
