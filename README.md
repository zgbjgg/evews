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

To start the evews websocket server just add the ebin/ path, process that controls the sokcte is designed as a 
non-blocking socket (tcp/ssl) to accept many concurrents connections at the same time, to start supervisor, it 
needs the next args:

		* Port 	   - the port where websocket runs
		* Ws Handler - the module and function that manages the websocket (as a callback module),
			     		here you can receive & send messages
		* Ssl	   - Certfile, Keyfile and password (if any)
		
this is a simple example how to start evews websocket server on port 8081 and the callback module example:loop/1 :

		evews_sup:start_link([{port, 8081}, {ws_handler, [{callback_m, example}, {callback_f, loop}]}]).
		
the callback module 'example' must have a function named loop which receives one parameter, the websocket
parametrized module, and this function is a simple process that receives messages from the broswer with the 
tuple '{browser, Data}', callback module function looks like this:

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
    		  
check the [evews_example.erl](https://github.com/jorgegarrido/evews/blob/master/examples/evews_example.erl) module for more info

exports
======

Evews Websocket is managed by a parametrized module, the next are the options in Ws:


		Ws:get(Data)
		Gets the message

		Ws:send(Msg)
		Sends the message to the browser, Msg can be a string or iolist

		Ws:socket()
		Returns the port for this socket

		Ws:peername()
		Returns the address and port for the other end of a connection

		Ws:port()
		Returns the local port number of this socket

		Ws:sockname()
		Returns the local address and port number of this socket.
		
LICENSE
======

THIS SOFTWARE IS LICENSED UNDER BSD LICENSE. see LICENSE.txt for more info
