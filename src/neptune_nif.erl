%%%-------------------------------------------------------------------
%%% @author Graham Crowe <graham.crowe@telia.com>
%%% @copyright (C) 2022, Graham Crowe
%%% @doc Neptune NIF Module
%%%
%%% demonstrates a trivial example using a Native Implemented Function
%%% (NIF) written in C and integrated with the Erlang Runtime System.
%%% @end
%%% Created :  4 Jan 2022 by Graham Crowe <graham.crowe@telia.com>
%%%-------------------------------------------------------------------
-module(neptune_nif).

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

-endif.

-on_load(init/0).

-export([square/1]).

%% -------------------------------------------------------------------
%% On load functions
%% -------------------------------------------------------------------
%% @doc Initialize the module on loading.
%%

-spec init() -> ok.

init() ->
    PrivDir = case code:priv_dir(?MODULE) of
		  {error, bad_name} ->
		      BeamPath = code:where_is_file("neptune_nif.beam"),
		      BeamDir = filename:dirname(BeamPath),
		      AppDir = filename:dirname(BeamDir),
		      filename:join(AppDir, "priv");
		  Dir ->
		      Dir
	      end,
    Path = filename:join(PrivDir, "libneptune_nif"),
    ok = erlang:load_nif(Path, 0).

%% -------------------------------------------------------------------

%% -------------------------------------------------------------------
%% Exported functions
%% -------------------------------------------------------------------
%% @doc Square an integer.
%%

-spec square(integer()) -> integer().

square(_Y) ->
    exit(nif_library_not_loaded).

%% -------------------------------------------------------------------

%% -------------------------------------------------------------------
%% Internal eunit tests
%% -------------------------------------------------------------------
-ifdef(TEST).

square_test_() ->
    [?_assertMatch(9,  square(3)),
     ?_assertMatch(36, square(6))].

-endif.
%% -------------------------------------------------------------------
