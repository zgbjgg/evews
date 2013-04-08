evews
=====

evews - Lightweight Websocket RFC 6455 - v1.9

download & build
====

Clone the evews repo:

		git clone https://github.com/jorgegarrido/evews.git
		
To compile you will need 'rebar', with debug logging enabled:

		make debug=on compile

Or without debug (option for not create a long messages in the logs)

		make compile


start
====

To start the evews websocket server just add it to your paths:

		erl -pa /path/evews/ebin/
		
now start the supervisor with the needed args:

		Port 	   - the port where websocket runs
		Ws Handler - the module and function that manages the websocket (as a callback module),
			     here you can receive & send messages
			     
this is a simple example how to start evews websocket server:

		evews_sup:start_link([{port, Port}, {ws_handler, [{callback_m, MODULE}, {callback_f, FUNCTION}]}]).
		
and the callback module function looks like this:

		loop(Ws) ->
    		    receive
        	 	{browser, Data} ->
            			io:format("receive ~p\n", [Ws:get(Data)]),
            			loop(Ws);
        		Any ->
            			io:format("any ~p\n", [Any]),
            			loop(Ws)
        		after 1000 ->
            			Ws:send(["pushing!"]),
            			loop(Ws)
    		    end.
    		  
check the [evews_example.erl]() module for more info

exports
======

Evews Websocket is managed by a parametrized module, the next are the options in Ws:


Ws:get(Data)
=========
Parse frame and get the message

Ws:send(Msg)
=========
Sends the message to the browser, Msg can be a string or iolist

Ws:socket()
=========
Returns the port for this socket

Ws:peername()
=========
Returns the address and port for the other end of a connection

Ws:port()
=========
Returns the local port number of this socket

Ws:sockname()
=========
Returns the local address and port number of this socket.
