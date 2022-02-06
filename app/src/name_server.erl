%%%-------------------------------------------------------------------
%%% @author %AUTHOR% %EMAIL%
%%% @copyright (C) %YEAR%, %AUTHOR%
%%% @doc %TC_PACKAGE_NAME% NIF Module
%%%
%%% demonstrates a trivial example of a module using the generic
%%% server behaviour.
%%% @end
%%% Created :  %DATE% by %AUTHOR% %EMAIL%
%%%-------------------------------------------------------------------
-module(%LC_PACKAGE_NAME%_server).

-behaviour(gen_server).

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

-endif.

-include("%LC_PACKAGE_NAME%.hrl").
-include("%LC_PACKAGE_NAME%_priv.hrl").

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
-spec start_link() ->
	  {ok, Pid} |
	  {error, {already_started, pid()}} |
	  {error, Reason} |
	  ignore when
      Pid    :: pid(),
      Reason :: term().
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%--------------------------------------------------------------------
%% @doc
%% Return the application version
%% @end
%%--------------------------------------------------------------------
-spec version() -> {ok, Version} when
      Version :: string().
version() ->
    gen_server:call(?SERVER, version).

%%--------------------------------------------------------------------
%% @doc
%% Construct a message
%% @end
%%--------------------------------------------------------------------
-spec message(Request) -> {ok, Response} | {error, Reason} when
      Request  :: {square, integer()},
      Response :: io_lib:chars(),
      Reason   :: term().
message({Type, Integer}) ->
    gen_server:call(?SERVER, {Type, Integer}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%% @end
%%--------------------------------------------------------------------
-spec init(Args :: term()) ->
	  {ok, State} |
	  {ok, State, Timeout} |
	  {ok, State, hibernate} |
	  {stop, Reason} |
	  ignore when
      State   :: term(),
      Timeout :: timeout(),
      Reason  :: term().
init([]) ->
    process_flag(trap_exit, true),
    {ok, #state{count = 0}, ?TIMEOUT}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%% @end
%%--------------------------------------------------------------------
-spec handle_call(Request, From, State) ->
	  {reply, Reply, NewState} |
	  {reply, Reply, NewState, Timeout} |
	  {reply, Reply, NewState, hibernate} |
	  {noreply, NewState} |
	  {noreply, NewState, Timeout} |
	  {noreply, NewState, hibernate} |
	  {stop, Reason, Reply, NewState} |
	  {stop, Reason, NewState} when
      Request  :: term(),
      From     :: {pid(), term()},
      State    :: term(),
      Reply    :: term(),
      NewState :: term(),
      Timeout  :: timeout(),
      Reason   :: term().
handle_call(version, _From, State) ->
    {ok, Application} = application:get_application(),
    Reply = application:get_env(Application, version),
    {reply, Reply, State, ?TIMEOUT};
handle_call({square, Integer}, _From, State) ->
    Reply = message(square, Integer),
    {reply, Reply, State, ?TIMEOUT};
handle_call(_Request, _From, State) ->
    {reply, {error, unsupported_request}, State, ?TIMEOUT}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%% @end
%%--------------------------------------------------------------------
-spec handle_cast(Request, State) ->
	  {noreply, NewState} |
	  {noreply, NewState, Timeout} |
	  {noreply, NewState, hibernate} |
	  {stop, Reason, NewState} when
      Request  :: term(),
      State    :: term(),
      NewState :: term(),
      Timeout  :: timeout(),
      Reason   :: term().
handle_cast(_Request, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%% @end
%%--------------------------------------------------------------------
-spec handle_info(Info, State) ->
	  {noreply, NewState} |
	  {noreply, NewState, Timeout} |
	  {noreply, NewState, hibernate} |
	  {stop, Reason, NewState} when
      Info     :: timeout() | term(),
      State    :: term(),
      NewState :: term(),
      Timeout  :: timeout(),
      Reason   :: normal | term().
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
-spec terminate(Reason,	State) -> any() when
      Reason :: normal | shutdown | {shutdown, term()} | term(),
      State  :: term().
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%% @end
%%--------------------------------------------------------------------
-spec code_change(OldVsn, State, Extra) ->
	  {ok, NewState} | {error, Reason} when
      OldVsn   :: term() | {down, term()},
      State    :: term(),
      Extra    :: term(),
      NewState :: term(),
      Reason   :: term().
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
-spec format_status(Opt, Status) -> Status when
      Opt    :: normal | terminate,
      Status :: list().
format_status(_Opt, Status) ->
    Status.

%%%===================================================================
%%% Internal functions
%%%===================================================================
-spec message(Type, Integer) -> {ok, Response} | {error, Reason}  when
      Type     :: atom(),
      Integer  :: integer(),
      Response :: io_lib:chars(),
      Reason   :: term().
message(square, Integer)
  when is_integer(Integer) ->
    {ok, Application} = application:get_application(),
    Place = atom_to_binary(Application),
    Greeting = <<"Hello from ", Place/binary, "!">>,
    Squared = %LC_PACKAGE_NAME%_nif:square(Integer),
    {ok, io_lib:format("~s ~p squared is ~p.~n", [Greeting, Integer, Squared])};
message(square, _Value) ->
    {error, not_an_integer}.
