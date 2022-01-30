%%%-------------------------------------------------------------------
%%% @author Graham Crowe <graham.crowe@telia.com>
%%% @copyright (C) 2022, Graham Crowe
%%% @doc Neptune application
%%%
%%% creates a minimal Erlang application.
%%% @end
%%% Created :  30 Jan 2022 by Graham Crowe <graham.crowe@telia.com>
%%%-------------------------------------------------------------------
-module(neptune_app).

-export([create/1]).

%%--------------------------------------------------------------------
%% @doc
%% This function creates an Erlang application based upon the input
%% expressed in the form of a map. It results in a minimal Erlang
%% application directory structure, built with GNU Autotools.
%% @end
%%--------------------------------------------------------------------
-spec create(Args :: map()) -> ok | {error, Reason :: binary()}.
create(_Args) ->
    ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================
