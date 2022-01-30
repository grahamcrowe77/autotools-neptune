%%%-------------------------------------------------------------------
%%% @author Graham Crowe <graham.crowe@telia.com>
%%% @copyright (C) 2022, Graham Crowe
%%% @doc Neptune substitutions
%%%
%%% provides regular expressions and replacements for all template
%%% substitutions.
%%% @end
%%% Created :  30 Jan 2022 by Graham Crowe <graham.crowe@telia.com>
%%%-------------------------------------------------------------------
-module(neptune_subs).

-export([main/1]).

%%--------------------------------------------------------------------
%% @doc
%% This function returns a tuple list of template substitutions
%% expressed as a tuple list of regular expressions and replacements.
%% @end
%%--------------------------------------------------------------------
main(Pars) ->
    [lowercase_package_name(Pars),
     uppercase_package_name(Pars),
     titlecase_package_name(Pars),
     package_version(Pars),
     erts_version(Pars),
     email(Pars),
     author(Pars),
     year(Pars),
     date(Pars)].

lowercase_package_name(#{name := Name}) ->
    {<<"%LC_PACKAGE_NAME%">>, string:lowercase(Name)}.

uppercase_package_name(#{name := Name}) ->
    {<<"%UC_PACKAGE_NAME%">>, string:uppercase(Name)}.

titlecase_package_name(#{name := Name}) ->
    {<<"%TC_PACKAGE_NAME%">>, string:titlecase(Name)}.

package_version(Pars) ->
    {<<"%PACKAGE_VERSION%">>, maps:get(version, Pars, <<"0.1.0">>)}.

erts_version(Pars) ->
    {<<"%ERLANG_ERTS_VER%">>, maps:get(erts_version, Pars, <<"11">>)}.

email(Pars) ->
    {<<"%EMAIL%">>, maps:get(email, Pars, <<"unknown@unknown.org">>)}.

author(Pars) ->
    {<<"AUTHOR">>, maps:get(author, Pars, <<"unknown">>)}.

year(_Pars) ->
    {{Year, _Month, _Day}, _Time} = calendar:local_time(),
    {<<"YEAR">>, integer_to_binary(Year)}.

date(_Pars) ->
    {{Year, Month, Day}, _Time} = calendar:local_time(),
    {<<"DATE">>,
     <<(integer_to_binary(Day))/binary, 32,
       (to_enum(month, Month))/binary, 32,
       (integer_to_binary(Year))/binary>>}.

to_enum(month, 1)  -> <<"Jan">>;
to_enum(month, 2)  -> <<"Feb">>;
to_enum(month, 3)  -> <<"Mar">>;
to_enum(month, 4)  -> <<"Apr">>;
to_enum(month, 5)  -> <<"May">>;
to_enum(month, 6)  -> <<"Jun">>;
to_enum(month, 7)  -> <<"Jul">>;
to_enum(month, 8)  -> <<"Aug">>;
to_enum(month, 9)  -> <<"Sep">>;
to_enum(month, 10) -> <<"Oct">>;
to_enum(month, 11) -> <<"Nov">>;
to_enum(month, 12) -> <<"Dec">>.
