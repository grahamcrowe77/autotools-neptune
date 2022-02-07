%%%-------------------------------------------------------------------
%%% @author %AUTHOR% %EMAIL%
%%% @copyright (C) %YEAR%, %AUTHOR%
%%% @doc %TC_APP_NAME% NIF Module
%%%
%%% demonstrates a trivial example using a Native Implemented Function
%%% (NIF) written in C and integrated with the Erlang Runtime System.
%%% @end
%%% Created :  %DATE% by %AUTHOR% %EMAIL%
%%%-------------------------------------------------------------------
-module(%LC_APP_NAME%_nif).

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
    Path = filename:join(priv_dir(), "lib%LC_APP_NAME%_nif"),
    ok = erlang:load_nif(Path, 0).

%% -------------------------------------------------------------------

%% -------------------------------------------------------------------
%% Exported functions
%% -------------------------------------------------------------------
%% @doc Square an integer.
%%

-spec square(X) -> Y when
      X :: integer(),
      Y :: integer().

square(_Y) ->
    exit(nif_library_not_loaded).

%% -------------------------------------------------------------------

%% -------------------------------------------------------------------
%% Internal
%% -------------------------------------------------------------------
priv_dir() ->
    case code:lib_dir(%LC_APP_NAME%, priv) of
	{error, bad_name} ->
	    BeamPath = code:where_is_file("%LC_APP_NAME%_nif.beam"),
	    BeamDir = filename:dirname(BeamPath),
	    AppDir = filename:dirname(BeamDir),
	    filename:join(AppDir, "priv");
	Dir ->
	    Dir
    end.

%% -------------------------------------------------------------------
%% Internal eunit tests
%% -------------------------------------------------------------------
-ifdef(TEST).

square_test_() ->
    [?_assertMatch(9,  square(3)),
     ?_assertMatch(36, square(6))].

-endif.
%% -------------------------------------------------------------------
