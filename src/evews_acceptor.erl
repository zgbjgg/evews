
-module(evews_acceptor).

-export([accept/1, accept_ws/1, ws_loop/2]).

-record(state, {port, loop, ip=any, lsocket=null, ws_handler}).

accept(State = #state{lsocket=LSocket, loop = Loop, ws_handler=WsHandler}) ->
    proc_lib:spawn(?MODULE, accept_ws, [{self(), LSocket, Loop, WsHandler}]),
    State.

accept_ws({Server, LSocket, {M, F}, WsHandler}) ->
    {ok, Socket} = gen_tcp:accept(LSocket),
    gen_server:cast(Server, {accepted, self()}),
    M:F(Socket, WsHandler).

ws_loop(Socket, {M, F}) ->
    % io:format("fun() ~p:~p() end", [M,F]),
    ok = inet:setopts(Socket, [{active, once}]),
    receive
    {tcp, Socket, Data} ->
	% error_logger:info_msg("Incoming message : ~p ~n", [Data]),
	ok = evews_handshake:handshake(Socket, Data),
        Ws = evews:new(Socket), 
	Pid = spawn_link(fun() -> M:F(Ws) end),
	loop(Socket, Pid);
	% ws_loop(Socket);
    {tcp_closed, _}->
        io:format("Socket closed~n");
    {tcp_error, Socket, Reason} ->
        io:format("Error on socket ~p reason: ~p~n", [Socket, Reason])
    end.

loop(Socket, Pid) ->
    ok = inet:setopts(Socket, [{active, once}]),
    receive
        {tcp, Socket, Data} ->
	    Pid ! {browser, incomming, Data},
	    loop(Socket, Pid);
	{tcp_closed, Socket} ->
 	    Pid ! {browser_closed, self()};
	{send, Data} ->
	  %  io:format("ok \n"),
 	    gen_tcp:send(Socket, [0,Data,255]),
 	    loop(Socket, Pid);
	Any ->
 	    io:format("Received:~p~n",[Any]),
 	    loop(Socket, Pid)
    end.


%% this sanity is for get a disconnect from a client
%% case Opcode of 8 -> gen_tcp:close(Socket), close; _ ->
