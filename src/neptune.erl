%% @author Graham Crowe <graham.crowe@telia.com>
%% @copyright 2021 Graham Crowe
%% @doc This module does nothing useful.
%%
%% Its purpose is to serve as an example for using GNU Autotools to
%% build an Erlang Application.
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
%% @equiv main(<<"Hello">>)
%%
%% @see main/1

-spec main() -> ok.

main() ->
    io:format("~s~n", [greeting(?DEFAULT_SALUTATION)]).

%% @doc Write a user defined greeting to the IoDevice.
%%

-spec main(Salutation :: binary()) -> ok | {error, Error :: atom()}.

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
%%

-spec greeting(Salutation :: binary()) -> binary().

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
