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

-export([main/0, main/1]).

%% -------------------------------------------------------------------
%% Exported functions
%% -------------------------------------------------------------------
%% @doc Write a default greeting to the IoDevice.

-spec main() -> ok | {error, Error} when
      Error :: atom().

main() ->
    io:format("~s~n", [greeting(?DEFAULT_SALUTATION)]).

%% -------------------------------------------------------------------
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
%% Internal eunit tests
%% -------------------------------------------------------------------
-ifdef(TEST).

greeting_test_() ->
    [?_assertMatch(<<"Hello from neptune!">>, greeting(<<"Hello">>)),
     ?_assertMatch(<<"Hi there from neptune!">>, greeting(<<"Hi there">>))].

-endif.
%% -------------------------------------------------------------------
