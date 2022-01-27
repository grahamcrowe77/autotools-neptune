%%%-------------------------------------------------------------------
%%% @author Graham Crowe <graham.crowe@telia.com>
%%% @copyright (C) 2022, Graham Crowe
%%% @doc Neptune Server Module
%%%
%%% demonstrates a trivial example of a module using the generic
%%% server behaviour.
%%% @end
%%% Created :  4 Jan 2022 by Graham Crowe <graham.crowe@telia.com>
%%%-------------------------------------------------------------------
-module(neptune_server).

-behaviour(gen_server).

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

-endif.

-include("neptune.hrl").
-include("neptune_priv.hrl").

%% API
-export([start_link/0, version/0, message/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3, format_status/2]).

-define(SERVER, ?MODULE).
-define(TIMEOUT, 5000).

-record(state, {count :: integer()}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%% @end
%%--------------------------------------------------------------------
-spec start_link() -> {ok, Pid :: pid()} |
	  {error, Error :: {already_started, pid()}} |
	  {error, Error :: term()} |
	  ignore.
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%--------------------------------------------------------------------
%% @doc
%% Return the application version
%% @end
%%--------------------------------------------------------------------
-spec version() -> {ok, string()}.
version() ->
    gen_server:call(?SERVER, version).

%%--------------------------------------------------------------------
%% @doc
%% Construct a message
%% @end
%%--------------------------------------------------------------------
-spec message({Type :: atom(), Number :: integer()}) -> {ok, string()}.
message({Type, Number}) ->
    gen_server:call(?SERVER, {Type, Number}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%% @end
%%--------------------------------------------------------------------
-spec init(Args :: term()) -> {ok, State :: term()} |
	  {ok, State :: term(), Timeout :: timeout()} |
	  {ok, State :: term(), hibernate} |
	  {stop, Reason :: term()} |
	  ignore.
init([]) ->
    process_flag(trap_exit, true),
    {ok, #state{count = 0}, ?TIMEOUT}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%% @end
%%--------------------------------------------------------------------
-spec handle_call(Request :: term(), From :: {pid(), term()}, State :: term()) ->
	  {reply, Reply :: term(), NewState :: term()} |
	  {reply, Reply :: term(), NewState :: term(), Timeout :: timeout()} |
	  {reply, Reply :: term(), NewState :: term(), hibernate} |
	  {noreply, NewState :: term()} |
	  {noreply, NewState :: term(), Timeout :: timeout()} |
	  {noreply, NewState :: term(), hibernate} |
	  {stop, Reason :: term(), Reply :: term(), NewState :: term()} |
	  {stop, Reason :: term(), NewState :: term()}.
handle_call(version, _From, State) ->
    {ok, Application} = application:get_application(),
    Reply = application:get_env(Application, version),
    {reply, Reply, State, ?TIMEOUT};
handle_call({square, Integer}, _From, State) ->
    Reply = {ok, message(square, Integer)},
    {reply, Reply, State, ?TIMEOUT}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%% @end
%%--------------------------------------------------------------------
-spec handle_cast(Request :: term(), State :: term()) ->
	  {noreply, NewState :: term()} |
	  {noreply, NewState :: term(), Timeout :: timeout()} |
	  {noreply, NewState :: term(), hibernate} |
	  {stop, Reason :: term(), NewState :: term()}.
handle_cast(_Request, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%% @end
%%--------------------------------------------------------------------
-spec handle_info(Info :: timeout() | term(), State :: term()) ->
	  {noreply, NewState :: term()} |
	  {noreply, NewState :: term(), Timeout :: timeout()} |
	  {noreply, NewState :: term(), hibernate} |
	  {stop, Reason :: normal | term(), NewState :: term()}.
handle_info(timeout, #state{count = Count}=State) ->
    Integer = (Count rem 10) + 1,
    Message = message(square, Integer),
    ok = io:format("~s~n", [Message]),
    {noreply, State#state{count = Count + 1}, ?TIMEOUT};
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%% @end
%%--------------------------------------------------------------------
-spec terminate(Reason :: normal | shutdown | {shutdown, term()} | term(),
		State :: term()) -> any().
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%% @end
%%--------------------------------------------------------------------
-spec code_change(OldVsn :: term() | {down, term()},
		  State :: term(),
		  Extra :: term()) -> {ok, NewState :: term()} |
	  {error, Reason :: term()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called for changing the form and appearance
%% of gen_server status when it is returned from sys:get_status/1,2
%% or when it appears in termination error logs.
%% @end
%%--------------------------------------------------------------------
-spec format_status(Opt :: normal | terminate,
		    Status :: list()) -> Status :: term().
format_status(_Opt, Status) ->
    Status.

%%%===================================================================
%%% Internal functions
%%%===================================================================
-spec message(Type :: atom(), Integer :: integer()) -> io_lib:chars().
message(square, Integer)
  when is_integer(Integer) ->
    {ok, Application} = application:get_application(),
    Place = atom_to_binary(Application),
    Greeting = <<"Hello from ", Place/binary, "!">>,
    Squared = neptune_nif:square(Integer),
    io_lib:format("~s ~p squared is ~p.~n", [Greeting, Integer, Squared]);
message(square, Value) ->
    {ok, Application} = application:get_application(),
    Place = atom_to_binary(Application),
    Greeting = <<"Hello from ", Place/binary, "!">>,
    io_lib:format("~s ~p is not an integer!~n", [Greeting, Value]).
