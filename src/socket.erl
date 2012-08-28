-module(socket).

-vsn("0.1").

-author("Jorge Garrido <jorge.garrido@morelosoft.com>").

-behaviour(gen_server).

%% API
-export([start_link/0, start/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

%% Socket functionallity
-export([accept_loop/1, loop/1]).

-define(SERVER, ?MODULE).

-include("javerl.hrl").

-record(state, {port, loop, ip=any, lsocket=null}).

%%%===================================================================
%%% API
%%%===================================================================

%% @spec start(Port :: integer(), Loop :: {atom(), atom()}) ->
%%            {ok, Pid :: pid()} | {error, Error :: term()}
%% @doc Starts the server to init the socket.
-spec start(Port :: integer(), Loop :: {atom(), atom()}) ->
      {ok, Pid :: pid()} | {error, Error :: term()}.
start(Port, Loop) ->
    State = #state{port = Port, loop = Loop},
    gen_server:start_link({local, socket}, ?MODULE, State, []).

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initiates the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init(State = #state{port=Port}) ->
    case gen_tcp:listen(Port, ?TCP_OPTIONS) of
	{ok, LSocket} ->
	    NewState = State#state{lsocket = LSocket},
	    {ok, accept(NewState)};
	{error, Reason} ->
	    {stop, Reason}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast({accepted, _Pid}, State=#state{}) ->
    {noreply, accept(State)};
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @spec accept(State :: term()) -> term()
%% @doc Accept the socket and send to the accept loop.
-spec accept(#state{}) -> #state{}.
accept(State = #state{lsocket=LSocket, loop = Loop}) ->
    proc_lib:spawn(?MODULE, accept_loop, [{self(), LSocket, Loop}]),
    State.

%% @spec accept_loop({Server :: atom(), LSocket :: port(),
%%                   { M :: atom(), F :: atom()}}) -> term()
%% @doc Send to control loop to receive messages on socket.
-spec accept_loop({Server :: atom(), LSocket :: port(),
		  { M :: atom(), F :: atom()}}) -> term().
accept_loop({Server, LSocket, {M, F}}) ->
    {ok, Socket} = gen_tcp:accept(LSocket),
    gen_server:cast(Server, {accepted, self()}),
    M:F(Socket).

%% @spec loop(Sock :: port()) -> term()
%% @doc Echo back whatever data we receive on Socket.
-spec loop(Sock :: port()) -> ok.
loop(Sock) ->
    ok = inet:setopts(Sock, [{active, once}]),
    receive
    {tcp, Socket, Data} ->
        Message = binary_to_list(Data),
	error_logger:info_msg("Incoming message : ~p ~n", [Message]),
        ok = gen_tcp:send(Socket, "ok"),
        loop(Socket);
    {tcp_closed, Socket}->
        io:format("Socket ~p closed~n", [Socket]);
    {tcp_error, Socket, Reason} ->
        io:format("Error on socket ~p reason: ~p~n", [Socket, Reason])	
    end.
