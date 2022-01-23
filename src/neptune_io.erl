%%%-------------------------------------------------------------------
%%% @author Graham Crowe <graham.crowe@telia.com>
%%% @copyright (C) 2022, Graham Crowe
%%% @doc Neptune IO Module
%%%
%%% sole purpose is to write greetings binary string greetings to IO.
%%% @end
%%% Created :  4 Jan 2022 by Graham Crowe <graham.crowe@telia.com>
%%%-------------------------------------------------------------------
-module(neptune_io).

-include("neptune.hrl").
-include("neptune_limits.hrl").

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

-endif.

-export([write/0, write/1]).

%% -------------------------------------------------------------------
%% Exported functions
%% -------------------------------------------------------------------
%% @doc Write a default greeting to the IoDevice.
%% @equiv write(<<"Hello">>)
%%
%% @see write/1

-spec write() -> ok.

write() ->
    io:format("~s~n", [greeting(?DEFAULT_SALUTATION)]).

%% @doc Write a user defined greeting to the IoDevice.
%%

-spec write(Salutation :: binary()) -> ok | {error, Error :: atom()}.

write(Salutation)
  when size(Salutation) > ?MAX_SALUTATION_LENGTH ->
    {error, max_salutation_length};
write(Salutation) ->
    io:format("~s~n", [greeting(Salutation)]).

%% -------------------------------------------------------------------

%% -------------------------------------------------------------------
%% Internal functions
%% -------------------------------------------------------------------
%% @doc Return a greeting binary string.
%%

-spec greeting(Salutation :: binary()) -> binary().

greeting(Salutation) ->
    Place = atom_to_binary(neptune),
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
