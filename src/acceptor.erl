%% ----------------------------------------------
%%
%% Jorge Garrido <jorge.garrido@morelosoft.com>
%%
%% acceptor.erl
%%
%% ----------------------------------------------

-module(acceptor).

-export([accept/1, accept_loop/1, loop/1]).

-include("javerl.hrl").

-record(state, {port, loop, ip=any, lsocket=null}).

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
        {ok, Reply} = ?REPLY,
        ok = gen_tcp:send(Socket, Reply),
        loop(Socket);
    {tcp_closed, _}->
        io:format("Socket closed~n");
    {tcp_error, Socket, Reason} ->
        io:format("Error on socket ~p reason: ~p~n", [Socket, Reason])
    end.

