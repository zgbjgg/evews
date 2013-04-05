-module(loop).

-export([loop/1]).

loop(Ws) ->
    % io:format("peername ~p | sockname ~p | port: ~p | socket: ~p\n", [ Ws:peername(), Ws:sockname(), Ws:port(), Ws:socket()]),
    receive
	{browser, incomming, Data} ->
	    io:format("receive ~p\n", [Ws:get(Data)]),
	    loop(Ws);
	Any ->
	    io:format("~p\n", [Any]),
            loop(Ws)
        after 1000 ->
	    Ws:send(["pushing!"]),
	    loop(Ws)
    end.
